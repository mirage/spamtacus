val stream :
  ?bounds:int ->
  string Lwt_stream.t ->
  [ `Parse of
    (Mrmime.Header.t * unit Mrmime.Mail.t, [> `Msg of string ]) result Lwt.t ]
  * Spamtacus.partial Lwt_stream.t
