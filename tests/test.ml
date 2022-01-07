open Lwt.Infix

(* The training set mails are a particular format: they might have an
   additional first line that make the parsing goes wrong. In this
   case, we ignore the first line. *)
let invalid_first_line line =
  if String.sub line 0 5 = "From " then true else false

let first_line ic =
  Lwt_io.read_line_opt ic >|= function
  | None -> failwith "empty mail"
  | Some line -> if invalid_first_line line then None else Some line

let read ic =
  let first = ref true in
  let read_to_stream ic =
    if !first then (
      first := false;
      first_line ic >>= function
      | None ->
          Lwt_io.read_line_opt ic
          >|= Option.map (fun str -> (str ^ "\r\n", 0, String.length str))
      | Some str -> Lwt.return_some (str ^ "\r\n", 0, String.length str))
    else
      Lwt.catch
        (fun () ->
          Lwt_io.read_line_opt ic
          >|= Option.map (fun str -> (str ^ "\r\n", 0, String.length str)))
        (function _ -> Lwt.return_none)
  in
  Lwt.return (fun () ->
      read_to_stream ic >>= function
      | None -> Lwt.return_none
      | Some truc -> Lwt.return (Some truc))

let rank stream : Spaml.label Lwt.t =
  Spaml_mirage.rank stream >>= fun (label, _mail_stream) -> Lwt.return label

let print_result filename label =
  Format.printf "File %s is a %s@." filename
    (match label with `Ham -> "ham" | `Spam -> "spam")

type label = [ `Ham | `Spam | `Unknown | `Error ]

let ranks (labelled_filenames : ([ `Ham | `Spam ] * string) list) :
    ([ `Ham | `Spam ] * label) list Lwt.t =
  Lwt_list.map_s
    (fun (label, filename) ->
      Lwt.catch
        (fun () ->
          Lwt_io.open_file ~mode:Lwt_io.input filename >>= fun ic ->
          read ic >>= fun input ->
          rank input >>= fun res ->
          Lwt_io.close ic >>= fun _ -> Lwt.return (label, (res :> label)))
        (function
          | Spaml_mirage.ParsingError _str -> Lwt.return (label, `Error)
          | exn -> Lwt.fail exn))
    labelled_filenames

(* Extract all filenames in the training set directory.*)
let extract_filenames () =
  let readdir label dirpath =
    let dirpath' = Fpath.to_string dirpath in
    if Sys.is_directory dirpath' then
      Sys.readdir dirpath'
      |> Array.to_list
      |> List.map (fun f -> (label, dirpath' ^ f))
    else failwith "todo"
  in
  let spam = readdir `Spam (Fpath.of_string "mails/spam/" |> Result.get_ok) in
  let ham = readdir `Ham (Fpath.of_string "mails/ham/" |> Result.get_ok) in
  let filenames = spam @ ham in
  Format.printf "Number of files : %d@." (List.length filenames);
  filenames

(* Count the wrongfully labelled mails *)
type res = {
  e : int;
  (* error *)
  u : int;
  (* unknown *)
  tp : int;
  (* true positive = spams labelled as spam *)
  fp : int;
  (* false positive = hams labelled as spam *)
  tn : int;
  (* true negative = hams labelled as ham *)
  fn : int;
  (* false negative = spams labelled as ham *)
  spam : int;
  ham : int;
}

let get_perf result =
  List.fold_left
    (fun res (true_label, computed_label) ->
      match (true_label, computed_label) with
      | _, `Error -> { res with e = res.e + 1 }
      | `Spam, `Unknown -> { res with u = res.u + 1; spam = res.spam + 1 }
      | `Ham, `Unknown -> { res with u = res.u + 1; ham = res.ham + 1 }
      | `Spam, `Spam -> { res with tp = res.tp + 1; spam = res.spam + 1 }
      | `Spam, `Ham -> { res with fn = res.fn + 1; spam = res.spam + 1 }
      | `Ham, `Ham -> { res with tn = res.tn + 1; ham = res.ham + 1 }
      | `Ham, `Spam -> { res with fp = res.fp + 1; ham = res.ham + 1 })
    { u = 0; tp = 0; fp = 0; tn = 0; fn = 0; e = 0; spam = 0; ham = 0 }
    result

let print_res { e; u; tp; fp; tn; fn; spam; ham } =
  Format.printf "Spam %d and hams %d\n" spam ham;
  Format.printf "Error : %d\n" e;
  Format.printf "Unknown : %d\n" u;
  Format.printf "False positive : %d\n" fp;
  Format.printf "True positive : %d\n" tp;
  Format.printf "False negative : %d\n" fn;
  Format.printf "True negative : %d@." tn

let main () =
  Lwt_main.run
    ( ranks (*[ (`Ham, "mail") ]*) (extract_filenames ()) >>= fun res ->
      get_perf res |> print_res;
      Lwt.return () )
;;

main ()
