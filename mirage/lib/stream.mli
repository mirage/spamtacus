val rank :
  (unit -> (string * int * int) option Lwt.t) ->
  (unit -> (string * int * int) option Lwt.t) Lwt.t
