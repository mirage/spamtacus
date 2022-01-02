val serialize : Fpath.t -> string -> unit

type training_set = Spaml.training_set
val train_and_write_to_file : training_set -> output:Fpath.t -> unit

val rank :
  (unit -> (string * int * int) option Lwt.t) ->
  (unit -> (string * int * int) option Lwt.t) Lwt.t
