val rank :
  string Lwt_stream.t ->
  (Spamtacus.label * string Lwt_stream.t, [> `Msg of string ]) result Lwt.t
(** [rank input] consumes and analyses an input mail [input] and
    returns the label computed by the bayesian filter defined in
    {!Spamtacus_bayesian} and a copy of the input stream to be passed to the
    next unikernel. A [X-spamtacus] header is appended to the copy with the the
    value:
    {ul
    {- [yes] if it has been labelled as a spam}
    {- [no] if it has been labelled as a ham }
    {- [unknown] if it has been labelled as unknown}}
 *)
