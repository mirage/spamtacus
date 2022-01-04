open Lwt.Infix

let blit src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

(* TODO : add a function that will work on the main header, as the
   only way to be certain to have access to it (the emitter is not
   necessary called on it) is by working on them after parsing, in the
   mail tree.*)

(* [parse ~emitters data] takes some mail partial [data] as input
   ([`String str] or [`Eof]) and parse it bit by bit. Every time a bit
   of body is parsed, the emitter is called on it, with the header of
   the current part of the mail. Note that as the body can be received
   in several pieces, the [emitter] will be called each time with the
   same [header]. *)
let parse ~emitters =
  let parser = Mrmime.Mail.stream ~emitters in
  (* Initializing the parser *)
  let state = ref (Angstrom.Unbuffered.parse parser) in
  (* queue for parsing bit by bit with Angstrom *)
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  fun data ->
    match !state with
    | Angstrom.Unbuffered.Done (_, tree) -> `Done tree
    | Fail _ -> `Fail
    | Partial { committed; continue } ->
        Ke.Rke.N.shift_exn ke committed;
        if committed = 0 then Ke.Rke.compress ke;
        let () =
          match data with
          | `String "" -> ()
          | `String str ->
              Ke.Rke.N.push ke ~blit ~length:String.length ~off:0
                ~len:(String.length str) str;
              let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
              state :=
                continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete
          | `Eof -> (
              match Ke.Rke.N.peek ke with
              | [] -> state := continue Bigstringaf.empty ~off:0 ~len:0 Complete
              | [ slice ] ->
                  state :=
                    continue slice ~off:0 ~len:(Bigstringaf.length slice)
                      Complete
              | slice :: _ ->
                  state :=
                    continue slice ~off:0 ~len:(Bigstringaf.length slice)
                      Incomplete)
        in
        `Continue

type input = {
  stream : unit -> (string * int * int) option Lwt.t;
  copy_pusher : (string * int * int) option -> unit;
}

let create_input stream =
  let stream', copy_pusher = Lwt_stream.create () in
  ({ copy_pusher; stream }, stream')

let manage_input { copy_pusher; stream } () =
  stream () >>= function
  | None ->
      copy_pusher None;
      Lwt.return `Eof (* end-of-input *)
  | Some (str, _, _) as v ->
      copy_pusher v;
      Lwt.return (`String (str ^ "\r\n"))

let parse input =
  let stream_of_words, push_words = Lwt_stream.create () in
  let emitters headers =
    let stream, push = Lwt_stream.create () in
    let stream' =
      Lwt_stream.map (Filter_imp.partial_extract headers) stream
      |> Lwt_stream.flatten
    in
    let rec transfer () =
      Lwt_stream.get stream' >>= function
      | None -> Lwt.return ()
      | Some str ->
          push_words (Some str);
          transfer ()
    in
    Lwt.async transfer;
    (push, ())
  in
  let parse = parse ~emitters in
  let rec go () =
    (* In some cases, all data are not parsed at once by [parse] and
       [parse `Eof] stil returns `Continue, whereas there are no more
       data available on input.stream, creating an Lwt_stream.Closed
       exeception *)
    Lwt.catch (manage_input input) (function
      | Lwt_io.Channel_closed _ -> Lwt.return `Eof
      | exn -> Lwt.fail exn)
    >>= fun data ->
    match parse data with
    | `Continue -> go ()
    | `Done (header, t) ->
        push_words None;
        Lwt.return_ok (header, t, stream_of_words)
    | `Fail ->
        push_words None;
        Lwt.return_error (`Msg "Invalid email")
  in
  go ()

let stream_to_t (stream : 'a Lwt_stream.t) : unit -> 'a option Lwt.t =
 fun () -> Lwt_stream.get stream

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

let rank (input : unit -> (string * int * int) option Lwt.t) =
  let input, copy_stream = create_input input in
  parse input >>= fun r ->
  match r with
  | Ok (header, tree, stream_of_words) ->
      ( Filter_imp.instanciate (stream_to_t stream_of_words) (header, tree)
      >>= fun ranks ->
        let label = Filter_imp.classify ranks in
        match label with
        | `Spam ->
            let header_stream = build_spam_header true |> header_to_stream in
            Lwt.return (label, Lwt_stream.append header_stream copy_stream)
        | `Ham -> Lwt.return (label, copy_stream) )
      >>= fun (label, stream) ->
      Lwt.return (label, fun () -> Lwt_stream.get stream)
  | Error (`Msg s) ->
      Lwt.fail (failwith ("TODO : mirage/lib/stream.ml (Error: " ^ s ^ ")"))
