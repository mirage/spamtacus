open Implem

type training_set = Spaml.training_set

let serialize db_dir filename =
  let static_value_filename =
    Fpath.add_seg db_dir filename |> Fpath.add_ext "ml"
  in
  let oc = open_out (Fpath.to_string static_value_filename) in

  let serialize name (db : Bayesian.Database.db) =
    Serialize.serialize oc db_dir name db
  in
  BayesianBody.(
    let ic = open_in Fpath.(add_seg db_dir name |> to_string) in
    let db = Bayesian.Database.read ic in
    close_in ic;
    serialize name db);
  BayesianSubject.(
    let ic = open_in Fpath.(add_seg db_dir name |> to_string) in
    let db = Bayesian.Database.read ic in
    close_in ic;
    serialize name db);
  close_out oc

let build_spam_header is_spam =
  if is_spam then
    let field =
      (match Unstrctrd.of_string "true\r\n" with
       | Ok (_, s) -> s
       | _ -> failwith "Unstructured"
        :> Mrmime.Unstructured.t)
    in
    let header =
      Mrmime.(
        Header.empty
        |> Header.add
             (Field_name.of_string_exn "X_spam")
             Field.(Unstructured, field))
    in
    Some header
  else None

let header_to_stream header =
  let stream =
    match header with
    | None -> fun () -> None
    | Some header ->
        Prettym.to_stream ~new_line:"" Mrmime.Header.Encoder.header header
  in
  Lwt_stream.from (fun () ->
      match stream () with
      | Some str -> Lwt.return_some (str, 0, String.length str)
      | None -> Lwt.return_none)

exception ParsingError of string

let rank (input : unit -> (string * int * int) option Lwt.t) =
  let open Lwt.Infix in
  let input, copy_stream = Stream.create_input input in
  Stream.parse input >>= function
  | Ok (header, tree, stream_of_words) ->
      ( Implem.instanciate
          (fun () -> Lwt_stream.get stream_of_words)
          (header, tree)
      >>= fun ranks ->
        let label = Implem.classify ranks in
        match label with
        | `Spam ->
            let header_stream = build_spam_header true |> header_to_stream in
            Lwt.return (label, Lwt_stream.append header_stream copy_stream)
        | `Ham -> Lwt.return (label, copy_stream) )
      >>= fun (label, stream) ->
      Lwt.return (label, fun () -> Lwt_stream.get stream)
  | Error (`Msg s) ->
      Lwt.fail (ParsingError ("mirage/lib/stream.ml (Error: " ^ s ^ ")"))

let train_and_write_to_file = Implem.train_and_write_to_file
