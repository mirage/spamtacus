open Lwt.Infix

let read ic =
  Lwt_stream.from @@ fun () ->
  match input_line ic with
  | line -> Lwt.return (Some (line ^ "\r\n"))
  | exception End_of_file -> Lwt.return_none

let rank stream : Spamtacus.label Lwt.t =
  Spamtacus_mirage.rank stream >>= function
  | Ok (label, _) -> Lwt.return label
  | Error (`Msg err) -> invalid_arg err

let print_result filename label =
  Format.printf "File %s is a %s@." filename
    (match label with `Ham -> "ham" | `Spam -> "spam")

type label = [ `Ham | `Spam | `Unknown | `Error ]

let ranks (labelled_filenames : ([ `Ham | `Spam ] * string) list) :
    ([ `Ham | `Spam ] * label) list Lwt.t =
  Lwt_list.map_s
    (fun (label, filename) ->
      let ic = open_in filename in
      rank (read ic) >>= fun res ->
      close_in ic;
      Lwt.return (label, (res :> label)))
    labelled_filenames

(* Extract all filenames in the training set directory.*)
let extract_filenames dir =
  let readdir label dirpath =
    let dirpath' = Fpath.to_string dirpath in
    if Sys.is_directory dirpath' then
      Sys.readdir dirpath'
      |> Array.to_list
      |> List.map (fun f -> (label, dirpath' ^ "/" ^ f))
    else failwith "todo"
  in
  let spam = readdir `Spam (Fpath.add_seg dir "spam") in
  let ham = readdir `Ham (Fpath.add_seg dir "ham") in
  let filenames = spam @ ham in
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

let percent a b = float_of_int a /. float_of_int b *. 100.

let print_res { e; u; tp; fp; tn; fn; spam; ham } =
  Format.printf "\nTotal number of mails : %d (%d spams and %d)\n" (spam + ham)
    spam ham;
  Format.printf "Errors                : %d (unprocessed mails)\n" e;
  Format.printf "Unknowns              : %d (unconclusive filter score)\n" u;
  Format.printf "False positives       : %d (%0.2f %% of hams)\n" fp
    (percent fp ham);
  Format.printf "True positives        : %d (%0.2f %% of spams)\n" tp
    (percent tp spam);
  Format.printf "False negatives       : %d (%0.2f %% of spams)\n" fn
    (percent fn spam);
  Format.printf "True negatives        : %d (%0.2f %% of hams)@." tn
    (percent tn ham)

let run_ dir =
  Lwt_main.run
    ( ranks (extract_filenames dir) >>= fun res ->
      get_perf res |> print_res;
      Lwt.return () )

let training_set_dir = "../Mails_corpus/ts1" |> Fpath.of_string |> Result.get_ok

let easy_test_dir =
  "../Mails_corpus/easy_test_corpus" |> Fpath.of_string |> Result.get_ok

let hard_test_dir =
  "../Mails_corpus/hard_test_corpus" |> Fpath.of_string |> Result.get_ok

let default_run () =
  Format.printf "## On training corpus ##@.";
  run_ training_set_dir;
  Format.printf "\n\n## On easy corpus ##@.";
  run_ easy_test_dir;
  Format.printf "\n\n## On hard corpus ##@.";
  run_ hard_test_dir

let run dir =
  (match dir with Some dir -> run_ dir | None -> default_run ());
  `Ok 0

open Cmdliner

let testing_corpus_dir =
  let filename = Arg.conv (Fpath.of_string, Fpath.pp) in
  let doc =
    "The directory where the training set can be found. It must be divided \
     into two sub-directories $(i,spam) and $(i,ham)."
  in
  Arg.(
    value
    & pos ~rev:true 0 (some filename) None
    & info [] ~doc ~docv:"TRAINING_SET_PATH")

let cmd =
  let doc = "todo" in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) generate $(i,bayesian_filter/database/static_database.ml).";
    ]
  in
  let info = Cmd.info "generate" ~doc ~man in
  Cmd.v info Term.(ret (const run $ testing_corpus_dir))

let () = exit @@ Cmd.eval' cmd
