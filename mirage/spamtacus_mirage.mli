exception ParsingError of string

val rank :
  (unit -> (string * int * int) option Lwt.t) ->
  ([ `Spam | `Ham | `Unknown ] * (unit -> (string * int * int) option Lwt.t))
  Lwt.t
(** [rank input] consumes and analyses an input mail [input] and
   returns the label computed by the bayesian filter defined in
   {!Spamtacus_bayesian} and a copy of the input stream to be passed
   to the next unikernel. A [X-spam] header is appended to the copy
   with the the value:
  {ul
   {- [yes] if it has been labelled as a spam}
   {- [no] if it has been labelled as a ham }
   {- [unknown] if it has been labelled as unknown}}
 *)
