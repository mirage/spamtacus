(** Spamtacus provides tools to compose a personnalized
   machine-learning filter for mails. A filter is defined by a {b
   feature vector} (See {!FEATURE} for feature definition) and a {b
   classify} function (See {!DT}). The {!Filter} functor takes care of
   putting together these pieces provided by the user to produce a
   functionning filter.

   This filter abstraction has been built based on supervized learning
   model. Namely, a mail is classified based on examples: a set of
   labelled mails called the training set. The filter then works in
   two phases (1) training phase (2) labelling phase.

    The {b training phase} is done once to process the training set and
   build a database extracting from the training phase the required
   features. For example, the database of a naive Bayesian filter
   contains the most frequent words of the training set with their
   frequency in spams and hams.
     
    The {b labelling phase} consists on:
    {ul
    {- extracting from an incoming mail the filter features (see
    {!FEATURE.partial_extract} and {!FEATURE.extract_from_header_tree})}
    {- ranking it for each features (see {!FEATURE.rank})}
    {- classifying the mail based on the ranks (see {!Filter.classify})}}
        
   {!Spamtacus} has been written to be included in a MirageOs
   unikernel. For this reason, mails are supposed to be provided
   through a stream of type [unit -> partial option Lwt.t] (see
   {!Filter.instanciate}) and features are extracted chunk by chunk
   (see {!FEATURE.partial_extract}).  *)

(* TODO: make it user defined *)
type label = [ `Spam | `Ham ]

(* TODO: better abstraction (!) *)
type rank = float list
(** The output type of {!rank} functions. It represents a feature
   score. Usually it is a single probability. *)

type ranks = (string * rank) list
(** A list of named feature scores.*)

(** A feature describes what part of a mail is extracted and processed
   to rank it, as well as how to extract and the algorithm used to
   process it.

   For example, it can be words in the body processed with a bayesian
   algorithm or the value of a specific header that makes the mail
   non-desirable.

   Each feature also defines its own database type (see {!FEATURE.db})
   and functions to read/write it (see {!FEATURE.write_db} and
   {!FEATURE.read_db}).
*)
module type FEATURE = sig
  val name : string
  (** A unique identifier.*)

  type t
  (** Describe the part of the mail that is extracted and studied by the
   classification ML algorithm. *)

  val empty : t

  val partial_extract : Mrmime.Header.t -> string -> string list
(** [partial_extract header chunk] defines how to extract the
   important parts of a [chunk], depending of the corresponding
   [header]. The extracted strings are combined to form the feature by
   [add_partial].

  The input headers should not be used to extract some header values
   from it as each header will be passed multiple times for every chunks
   associated to it. Use [extract_from_header_tree] instead.  *)

  val extract_from_header_tree : Mrmime.Header.t * unit Mrmime.Mail.t -> t
(** [extract_from_header_tree (headers, tree)] can be used to extract
   feature from [headers]. [tree] describes the structure of the mail
   and can be traversed to access headers of multipart mail. *)

  val add_partial : t -> string list -> t

  type db
  (** [db] defines the database that needs to be registered for the
   feature. *)

  val empty_db : db

  val train : db -> label -> t -> db
(** [train db label t] adds the features [t] extracted from a mail
   labelled [label] to the dababase [db]. *)

  val write_db : out_channel -> db -> unit
  val read_db : in_channel option -> db

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
