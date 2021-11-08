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
  val db_to_json : string -> db -> unit
  val db_of_json : out_channel -> db
end

type feature_vector = (module FEATURE) list

let create_fv () : feature_vector = []

let add_feature (f : (module FEATURE)) (fv : feature_vector) : feature_vector =
  f :: fv

module type FV = sig
  val vector : feature_vector
end

module type DT = sig
  type model = (string * float list) list

  val classify : model -> label
end

module type MACHINE = functor (Features : FV) (DecisionTree : DT) -> sig
  type model = DecisionTree.model
  type filename = string

  val train_and_write : filename -> (label * mail) list -> unit
  val instanciation : filename -> mail -> model
  val classify : model -> label
end

module Machine (Features : FV) (DecisionTree : DT) = struct
  type model = DecisionTree.model
  type filename = string

  let build_filename dirname (module Feature : FEATURE) =
    dirname ^ "/" ^ Feature.name

  let train_and_write dirname mails =
    List.iter
      (fun (module F : FEATURE) ->
        let oc = open_out (build_filename dirname (module F)) in
        F.(train mails |> write_db oc);
        close_out oc)
      Features.vector

  let instanciation dirname mail : model =
    let model =
      List.fold_left
        (fun acc (module F : FEATURE) ->
          let ic = open_in (build_filename dirname (module F)) in
          let db = F.read_db ic in
          close_in ic;
          (F.name, F.rank mail db) :: acc)
        [] Features.vector
    in
    model

  let classify : model -> label = DecisionTree.classify
end
