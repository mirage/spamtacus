val stream :
  ?bounds:int ->
  string Lwt_stream.t ->
  [ `Parse of
    (Mrmime.Header.t * unit Mrmime.Mail.t, [> `Msg of string ]) result Lwt.t ]
  * Spamtacus.partial Lwt_stream.t
(** [stream ?bounds input] returns a fiber which parse the email and a stream
    of {!type:Spamtacus.partial} values which can be used by a {i filter}. *)
