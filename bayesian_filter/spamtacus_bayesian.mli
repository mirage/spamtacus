open Spamtacus

(** This filter uses two features:
  {ul
    {- a naive bayesian algorithm that works on mail body. }
    {- a naive bayesian algorithm that works on words contained
    in [subject] header value. }}

    The decision tree of this filter is quite simple: both features return
    a probability of being a spam. The label is:
    {ul
    {- [`Spam] if any of the features returns a probability higher than 0.7}
    {- [`Ham] otherwise.}}
     
*)

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
