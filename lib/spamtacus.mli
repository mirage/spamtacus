(** Spamtacus provides tools to compose a personnalized
   machine-learning filter for mails. A filter is defined by a {b
   feature vector} (See {!FEATURE} for feature definition) and a {b
   classify} function (See {!DT}). The {!Filter} functor takes care of
   putting together these pieces provided by the user to produce a
   functionning filter.

    {!Spamtacus} has been written to be included in a MirageOs
   unikernel. For this reasons, mails are supposed to be provided
   through a stream of type [unit -> partial option Lwt.t] (see
   {!Filter.instanciate}) and features are extracted chunk by chunk
   (see {!FEATURE.partial_extract}).
*)

(* TODO: make it user defined *)
type label = [ `Spam | `Ham ]

(* TODO: better abstraction (!) *)
type rank = float list
(** The output type of {!rank} functions. It represents a feature
   score. Usually it is a single probability. *)

type ranks = (string * rank) list
(** A list of named feature scores.*)

type partial = { name : string; extracted : string list }
(** Describes the parts extracted from received mails chunks.  *)

(** A feature describes the parts of a mail that are extracted and
   processed to rank the mail, as well as how to extract and process
   it.

   It can be some words in the body or the value of a specific header
   for example.  *)
module type FEATURE = sig
  val name : string
  (** A unique identifier.*)

  type t
  (** Describe the part of the mail that is extracted and studied by the
   classification ML algorith. *)

  val empty : t

  (* Extraction functions. *)
  val partial_extract : Mrmime.Header.t -> string -> partial option
  (** [partial_extract header chunk] defines how to extract the
   important parts of a chunk, depending of the corresping
   [header]. The extracted string are combined to form the feature by
   [add_partial].

  The input headers should not be used to extract some header value
   from it as it will be passed multiple times for every chunks
   associated to it. Use [extract_from_header_tree] instead.  *)

  val extract_from_header_tree : Mrmime.Header.t * unit Mrmime.Mail.t -> t
  (** [extract_from_header_tree (headers, tree)] *)

  val add_partial : t -> partial -> t
  (** [build_from_partial] defines how to build a feature of type [t]
   from partial parts extracted with [partial_extract]. *)

  (* Database related functions and type*)
  type db
  (** [db] defines the database that needs to be registered for the
   feature. *)

  val empty_db : db

  val train : db -> label -> t -> db
  (** [train labeled extracts db] *)

  val write_db : out_channel -> db -> unit
  val read_db : in_channel option -> db

  (* Ranking functions *)
  val rank : t -> db -> rank
  (** [rank t db]*)
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

type training_set = { spam : Fpath.t; ham : Fpath.t }

type partial = { name : string; extracted : string list }
(** Describes the parts extracted from received mails chunks. The
   [name] shoud be filled with the feature name.  *)

(** A Filter is the  by a feature vector defined byt the function and a decision tree   *)
module type FILTER = functor (Features : FV) (DecisionTree : DT) -> sig
  (* Training functions *)
  val train_and_write_to_file : training_set -> output:Fpath.t -> unit

  (* Extraction functions *)
  val partial_extract : Mrmime.Header.t -> string -> partial list

  (* Ranking functions *)
  val instanciate :
    ?input_dir:Fpath.t ->
    (unit -> partial option Lwt.t) ->
    Mrmime.Header.t * unit Mrmime.Mail.t ->
    ranks Lwt.t

  val classify : ranks -> label

  (* Utilitary function *)
  val get_features_name : unit -> string list
end

module Filter : FILTER
