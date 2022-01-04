val rank :
  (unit -> (string * int * int) option Lwt.t) ->
  ([`Spam | `Ham] * (unit -> (string * int * int) option Lwt.t)) Lwt.t
