open Spamacus

val train_and_write_to_file : training_set -> output:Fpath.t -> unit

val instanciate :
  ?input_dir:Fpath.t ->
  (unit -> partial option Lwt.t) ->
  Mrmime.Header.t * unit Mrmime.Mail.t ->
  ranks Lwt.t

val partial_extract : Mrmime.Header.t -> string -> partial list
val classify : ranks -> label
val serialize : Fpath.t -> string -> unit
