open Spamtacus

(** This filter uses three features:
  {ul
    {- a naive bayesian algorithm that works on mail body (See {!BayesianBody}). }
    {- a naive bayesian algorithm that works on [subject] header value (See {!BayesianSubject}). }
    {- a basic antivirus that rejects mails with forbidden attachment (`.exe` files
     for example) (See {!BasicAntiVirus}). }}

    The decision tree of this filter is quite simple:   
   {ul
    {- a mail is labelled as [`Spam] if it contains a forbidden attachment or if
    the probability of beeing a spam computed by any of the two bayesian filters
    is greater than 0.7,}
    {- if no forbidden attachment has been found and both probabilities of beeing a
    spam are lower than 0.3, it is labelled as [`Ham`].}
    {- Otherwise an `Unknown` label is returned.}}
*)

(** {1:bayesian_features Implemented features} *)

module BayesianBody : Spamtacus.FEATURE with type db = Database.db
(** Naive Bayesian filter on mail body content. *)

module BayesianSubject : Spamtacus.FEATURE with type db = Database.db
(** Naive Bayesian filter on subject header value. *)

module BasicAntiVirus : Spamtacus.FEATURE
(** Basic antivirus that  rejects mails with forbidden attachment. It looks for
  attachment file name in  :
  {ul
  {- [name] parameter of [content-type] headers}
  {- [filename] parameter of [content-disposition] headers}}
    in each part of an incoming mail. If the attachement filenames found have 
  a forbidden extension  the mail is given a rank of [1.].
 
  Forbidden extensions are [.ade], [.adp], [.bat], [.chm], [.cmd],
   [.com], [.cpl], [.exe], [.hta], [.ins], [isp], [.jse], [.lib],
   [.mde], [.msc], [.msp], [.mst], [.pif], [.scr], [.sct], [shb],
   [.sys], [.vb], [.vbe], [.vbs], [.vxd], [.wsc], [.wsf], [.wsh].  

  Note: This feature does not require any machine learning algorithm nor
 a database. *)

(** {1:filter_func Bayesian filter} *)

val train_and_write_to_file : training_set -> output:Fpath.t -> unit
(** [train_and_write_to_file training_set ~output] computes the
   combined database of each feature from the [training_set] and
   writes it in [output]. *)

val partial_extract : Mrmime.Header.t -> string -> partial list
val classify : ranks -> label

val instanciate :
  ?input_dir:Fpath.t ->
  (unit -> partial option Lwt.t) ->
  Mrmime.Header.t * unit Mrmime.Mail.t ->
  ranks Lwt.t
(** [instanciate ~input_dir stream (header, tree)] ranks each feature
   from an incoming mail [stream] mail and its header
   [(header,tree)]. The [input_dir] must contain the dabatase built
   with {!train_and_write_to_file}. *)

val serialize : Fpath.t -> string -> unit
