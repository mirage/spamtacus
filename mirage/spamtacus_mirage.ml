let build_spam_header label =
  let field =
    match label with
    | `Spam ->
        (match Unstrctrd.of_string "yes\r\n" with
         | Ok (_, s) -> s
         | _ -> failwith "Unstructured"
          :> Mrmime.Unstructured.t)
    | `Ham ->
        (match Unstrctrd.of_string "no\r\n" with
         | Ok (_, s) -> s
         | _ -> failwith "Unstructured"
          :> Mrmime.Unstructured.t)
    | `Unknown ->
        (match Unstrctrd.of_string "unknown\r\n" with
         | Ok (_, s) -> s
         | _ -> failwith "Unstructured"
          :> Mrmime.Unstructured.t)
  in
  Mrmime.(
    Header.empty
    |> Header.add (Field_name.of_string_exn "X_spam") Field.(Unstructured, field))

let header_to_stream header =
  let stream =
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
      ( Spamtacus_bayesian.instanciate
          (fun () -> Lwt_stream.get stream_of_words)
          (header, tree)
      >>= fun ranks ->
        let label = Spamtacus_bayesian.classify ranks in
        let header_stream = build_spam_header label |> header_to_stream in
        Lwt.return (label, Lwt_stream.append header_stream copy_stream) )
      >>= fun (label, stream) ->
      Lwt.return (label, fun () -> Lwt_stream.get stream)
  | Error (`Msg s) ->
      Lwt.fail (ParsingError ("mirage/lib/stream.ml (Error: " ^ s ^ ")"))
