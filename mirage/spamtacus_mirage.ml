let build_spam_header label =
  let field =
    match label with
    | `Spam -> snd (Result.get_ok (Unstrctrd.of_string "yes\r\n"))
    | `Ham -> snd (Result.get_ok (Unstrctrd.of_string "no\r\n"))
    | `Unknown -> snd (Result.get_ok (Unstrctrd.of_string "unknown\r\n"))
  in
  let open Mrmime in
  Header.add
    (Field_name.of_string_exn "X-Spamtacus")
    Field.(Unstructured, (field :> Unstructured.elt list))
    Header.empty

let header_to_stream header =
  let stream = Prettym.to_stream Mrmime.Header.Encoder.header header in
  Lwt_stream.from (fun () -> Lwt.return (stream ()))

let dup src =
  let open Lwt.Infix in
  let copy, push = Lwt_stream.create () in
  let dup () =
    Lwt_stream.get src >>= fun v ->
    push v;
    Lwt.return v
  in
  (Lwt_stream.from dup, copy)

let rank src =
  let open Lwt.Infix in
  let src, copy = dup src in
  let `Parse th, stream_of_words = Stream.stream src in
  th >>= function
  | Ok (headers, tree) ->
      Spamtacus_bayesian.instanciate
        (fun () -> Lwt_stream.get stream_of_words)
        (headers, tree)
      >>= fun ranks ->
      let label = Spamtacus_bayesian.classify ranks in
      let label_stream = build_spam_header label |> header_to_stream in
      Lwt.return_ok (label, Lwt_stream.append label_stream copy)
  | Error _ as err -> Lwt.return err
