open Lwt.Infix

let blit src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

(* [parse ~emitters data] takes some mail partial [data] as input
   ([`String str] or [`Eof]) and parse it bit by bit. Every time a bit
   of body is parsed, the emitter is called on it, with the header of
   the current part of the mail. Note that as the body can be received
   in several pieces, the [emitter] will be called each time with the
   same [header]. *)
let parse ~emitters =
  let parser = Mrmime.Mail.stream emitters in
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

let rec transfer_words wk src dst =
  Lwt_stream.get src >>= function
  | None ->
      Lwt.wakeup_later wk ();
      Lwt.return_unit
  | Some word ->
      dst (Some word);
      transfer_words wk src dst

and stream ?(bounds = 10) stream =
  let output, push = Lwt_stream.create () in
  let q = Queue.create () in
  let tbl = Hashtbl.create 0x10 in
  let fresh_id =
    let r = ref 0 in
    fun () ->
      incr r;
      !r
  in
  let emitters headers =
    let id = fresh_id () in
    Queue.push (`Headers (headers, id)) q;
    ((fun data -> Queue.push (`Data (id, data)) q), ())
  in
  let parse = parse ~emitters in
  let rec go ths =
    match Queue.pop q with
    | `Headers (headers, id) ->
        let stream, emitter = Lwt_stream.create_bounded bounds in
        let stream =
          Lwt_stream.map (Spamtacus_bayesian.partial_extract headers) stream
        in
        let stream = Lwt_stream.flatten stream in
        Hashtbl.add tbl id emitter;
        let th, wk = Lwt.task () in
        Lwt.async (fun () -> transfer_words wk stream push);
        go (th :: ths)
    | `Data (id, Some data) ->
        let push = Hashtbl.find tbl id in
        push#push data >>= fun () -> go ths
    | `Data (id, None) ->
        let push = Hashtbl.find tbl id in
        push#close;
        go ths
    | exception Queue.Empty -> (
        Lwt_stream.get stream >>= fun data ->
        let data = match data with Some str -> `String str | None -> `Eof in
        match parse data with
        | `Continue -> go ths
        | `Done (headers, tree) ->
            Lwt.join ths >>= fun () ->
            push None;
            Lwt.return_ok (headers, tree)
        | `Fail ->
            Lwt.join ths >>= fun () ->
            push None;
            Lwt.return_error (`Msg "Invalid email"))
  in
  (`Parse (go []), output)
(*




   type input = {
     stream : unit -> (string * int * int) option Lwt.t;
     copy_pusher : (string * int * int) option -> unit;
   }

   let create_input stream =
     let stream', copy_pusher = Lwt_stream.create () in
     ({ copy_pusher; stream }, stream')

   let manage_input { copy_pusher; stream } =
     stream () >>= function
     | None ->
         (try copy_pusher None with Lwt_stream.Closed -> ());
         Lwt.return `Eof (* end-of-input *)
     | Some (str, _, _) as v ->
         copy_pusher v;
         Lwt.return (`String (str ^ "\r\n"))

   let parse input =
     let stream_of_words, push_words = Lwt_stream.create () in
     let async_emitters = ref 1 in
     let emitters headers =
       incr async_emitters;
       let stream, push = Lwt_stream.create () in
       let stream' =
         Lwt_stream.map (Spamtacus_bayesian.partial_extract headers) stream
         |> Lwt_stream.flatten
       in
       let rec transfer () =
         Lwt_stream.get stream' >>= function
         | None ->
             decr async_emitters;
             if !async_emitters = 0 then push_words None;
             Lwt.return ()
         | Some str ->
             push_words (Some str);
             transfer ()
       in
       Lwt.async transfer;
       (push, ())
     in
     let parse = parse ~emitters in
     let rec go () =
       manage_input input >>= fun data ->
       match parse data with
       (* TODO: here the extracted data should be processed in a lwt promise
          to keep the memory use low*)
       | `Continue -> go ()
       | `Done (header, t) ->
           decr async_emitters;
           if !async_emitters = 0 then push_words None;
           Lwt.return_ok (header, t, stream_of_words)
       | `Fail -> Lwt.return_error (`Msg "Invalid email")
     in
     go ()
*)
