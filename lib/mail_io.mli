val parse :
  Fpath.t ->
  (Mrmime.Header.t * string Mrmime.Mail.t, [> `Msg of string ]) result
