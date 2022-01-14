(**
 Implementation of a bayesian filter applied to two features :

  - the main subject of the mail (meaning only the first headers
   are introspect to extract a subject)
   
 - all bodies included in the mail.   
*)

include Spamtacus

module BayesianBody : FEATURE with type db = Database.db = struct
  let name = "BayesianBody"

  type t = Extract.BagOfWords.t

  let empty = Extract.BagOfWords.empty

  type db = Database.db

  let empty_db = Database.create ()

  let partial_extract _ (str : string) : partial option =
    let extracted =
      Extract.extract str |> fun e ->
      Extract.BagOfWords.fold (fun w _ acc -> w :: acc) e []
    in
    match extracted with [] -> None | _ -> Some { name; extracted }

  let extract_from_header_tree _header_tree : t = Extract.BagOfWords.empty

  let add_partial (t : t) ({ extracted; _ } : partial) : t =
    List.fold_left (fun t word -> Extract.BagOfWords.add word t) t extracted

  let write_db oc db = Database.write oc db
  let read_db _ic = Static_database.bayesianBody

  let train db label bow : db =
    match label with
    | `Spam -> Database.add_spam bow db
    | `Ham -> Database.add_ham bow db

  let rank t db = [ Classify.rank ~max_word:20 t db ]
end

module BayesianSubject : FEATURE with type db = Database.db = struct
  let name = "BayesianMainSubject"

  type t = Extract.BagOfWords.t

  let empty = Extract.BagOfWords.empty

  type db = Database.db

  let empty_db = Database.create ()

  let extract_main_subject_value header =
    Mrmime.Header.assoc Mrmime.Field_name.subject header
    |> List.map (fun h -> Prettym.to_string Mrmime.Field.Encoder.field h)

  let partial_extract _ _ = None

  let extract_from_header_tree (header, tree) : t =
    let extract_and_add acc header =
      let main_subjects = extract_main_subject_value header in
      List.fold_left
        (fun bow subject ->
          let new_bow = Extract.extract subject in
          Extract.BagOfWords.union bow new_bow)
        acc main_subjects
    in
    let rec go acc = function
      | Mrmime.Mail.Leaf () -> acc
      | Message (h, t) ->
          let acc = extract_and_add acc h in
          go acc t
      | Multipart parts ->
          List.fold_left
            (fun acc (header, bodyopt) ->
              let acc = extract_and_add acc header in
              match bodyopt with None -> acc | Some t -> go acc t)
            acc parts
    in
    go (extract_and_add Extract.BagOfWords.empty header) tree

  let add_partial (t : t) ({ extracted; _ } : partial) : t =
    List.fold_left (fun t word -> Extract.BagOfWords.add word t) t extracted

  let write_db oc db = Database.write oc db
  let read_db _ic = Static_database.bayesianMainSubject

  let train db label t : db =
    match label with
    | `Spam -> Database.add_spam t db
    | `Ham -> Database.add_ham t db

  let rank t db = [ Classify.rank ~max_word:1 t db ]
end

let fv =
  create_fv ()
  |> add_feature (module BayesianBody)
  |> add_feature (module BayesianSubject)

module BayesianFilter =
  Machine (struct
      let vector = fv
    end)
    (struct
      let classify ranks =
        let subject_score = List.assoc BayesianSubject.name ranks |> List.hd in
        let body_score = List.assoc BayesianBody.name ranks |> List.hd in
        if subject_score > 0.7 then `Spam
        else if body_score > 0.7 then `Spam
        else `Ham
    end)

include BayesianFilter

let serialize db_dir filename =
  let static_value_filename =
    Fpath.add_seg db_dir filename |> Fpath.add_ext "ml"
  in
  let oc = open_out (Fpath.to_string static_value_filename) in

  let serialize name (db : Database.db) =
    Serialize.serialize oc db_dir name db
  in
  BayesianBody.(
    let ic = open_in Fpath.(add_seg db_dir name |> to_string) in
    let db = Database.read ic in
    close_in ic;
    serialize name db);
  BayesianSubject.(
    let ic = open_in Fpath.(add_seg db_dir name |> to_string) in
    let db = Database.read ic in
    close_in ic;
    serialize name db);
  close_out oc
