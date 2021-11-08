type mail = Mrmime.Header.t * string Mrmime.Mail.t
type label = [ `Spam | `Ham ]

module type FEATURE = sig
  type t
  (** A feature is a part of the mail that is extracted and studied by
      the ML algorith to classify it.  *)

  val name : string
  (** The unique identifier used to defined the feature.*)

  val extract : mail -> t
  (** [extract] is the function that defines how to extract the feature
   from the mail. *)

  (* this part should probably be out of the module somehow *)
  type db
  (** [db] defines the structure of the data we need to registered for
   this feature. For example, for a naive bayesian filter, we may
   simply need to register how many times each extracted words
   appeared in spams and in hams. *)

  val train : (label * mail) list -> db
  (** [train labeled_mails] *)

  val rank : mail -> db -> float list
  val write_db : out_channel -> db -> unit
  val read_db : in_channel -> db
end

type feature_vector
(** FEATURE VECTOR *)

val create_fv : unit -> feature_vector
val add_feature : (module FEATURE) -> feature_vector -> feature_vector

module type FV = sig
  val vector : feature_vector
end

(** Decision Tree *)
module type DT = sig
  type ranks = (string * float list) list

  val classify : ranks -> label
end

(** Machine *)
module type MACHINE = functor (Features : FV) (DecisionTree : DT) -> sig
  type ranks = DecisionTree.ranks
  type filename = string

  val train_and_write : filename -> (label * mail) list -> unit
  val instanciation : filename -> mail -> ranks
  val classify : ranks -> label
end

module Machine : MACHINE
