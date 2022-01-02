type label = [ `Spam | `Ham ]
type partial = { name : string; extracted : string list }
type header_tree = Mrmime.Header.t * unit Mrmime.Mail.t
type rank = float list
type ranks = (string * rank) list
type training_set = { spam : Fpath.t; ham : Fpath.t }

module type FEATURE = sig
  val name : string
  (** The unique identifier used to defined the feature.*)

  type t
  (** Describe the part of the mail that is extracted and studied by the
   classification ML algorith. *)

  val empty : t

  (* Extraction functions. *)
  val partial_extract : Mrmime.Header.t -> string -> partial option
  (** [partial_extract] defines how to extract some partial feature from
   a piece of body and the corresponding headers. *)

  val extract_from_header_tree : header_tree -> t

  val add_partial : t -> partial -> t
  (** [build_from_partial] defines how to build the feature from partial
   part extracted with [partial_extract]. *)

  (* Database related functions and type*)
  type db
  (** [db] defines the structure of the data we need to registered for
      this feature. For a naive bayesian filter, we may simply need to
      register how many times each extracted words appeared in spams and
      in hams. *)

  val empty_db : db

  val train : db -> label -> t -> db
  (** [train labeled extracts db] *)

  val write_db : out_channel -> db -> unit
  val read_db : in_channel option -> db

  (* Ranking functions *)
  val rank : t -> db -> rank
end

type feature_vector
(** FEATURE VECTOR *)

val create_fv : unit -> feature_vector
val add_feature : (module FEATURE) -> feature_vector -> feature_vector
val map_features : ((module FEATURE) -> 'a) -> feature_vector -> 'a list

module type FV = sig
  val vector : feature_vector
end

(** Decision Tree *)
module type DT = sig
  val classify : ranks -> label
end

(** Machine *)
module type MACHINE = functor (Features : FV) (DecisionTree : DT) -> sig

  (* Training functions *)
  val train_and_write_to_file : training_set -> output:Fpath.t -> unit

  (* Extraction functions *)
  val partial_extract : Mrmime.Header.t -> string -> partial list

  (* Ranking functions *)
  val instanciate :
    ?input_dir:Fpath.t ->
    (unit -> partial option Lwt.t) ->
    header_tree ->
    ranks Lwt.t

  val classify : ranks -> label

  (* Utilitary function *)
  val get_features_name : unit -> string list
end

module Machine : MACHINE
