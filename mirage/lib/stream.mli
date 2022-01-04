type input = {
  stream : unit -> (string * int * int) option Lwt.t;
  copy_pusher : (string * int * int) option -> unit;
}

val create_input :
  (unit -> (string * int * int) option Lwt.t) ->
  input * (string * int * int) Lwt_stream.t

val parse :
  input ->
  ( Mrmime.Header.t * unit Mrmime.Mail.t * Spaml.partial Lwt_stream.t,
    [ `Msg of string ] )
  result
  Lwt.t
