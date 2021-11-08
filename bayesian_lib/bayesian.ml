(**
 Implementation of a bayesian filter applied to two features :

  - the main subject of the mail (meaning only the first headers
   are introspect to extract a subject)
   
 - all bodies included in the mail.   
*)

open Spaml

module BayesianBody : FEATURE = struct
  let name = "BayesianBody"

  type t = Extract.WordSet.t
  type db = Database.db

  let extract mail = Extract.extract_bodies_words mail
  let write_db ic db = Database.write ic db
  let read_db oc = Database.read oc

  let train labeled_mails =
    List.fold_left
      (fun db (label, mail) ->
        match label with
        | `Spam -> Database.add_spam mail extract db
        | `Ham -> Database.add_ham mail extract db)
      (Database.create ()) labeled_mails

  let rank mail db = [ Classify.rank ~max_word:15 mail db ]
end

module BayesianSubject : FEATURE = struct
  let name = "BayesianMainSubject"

  type t = Extract.WordSet.t

  let extract = Extract.extract_main_subject_values
  let write_db ic db = Database.write ic db
  let read_db oc = Database.read oc

  type db = Database.db

  let train labeled_mails =
    List.fold_left
      (fun db (label, mail) ->
        match label with
        | `Spam -> Database.add_spam mail extract db
        | `Ham -> Database.add_ham mail extract db)
      (Database.create ()) labeled_mails

  let rank mail db = [ Classify.rank ~max_word:15 mail db ]
end

(* Let's try ! *)
let fv =
  create_fv ()
  |> add_feature (module BayesianBody)
  |> add_feature (module BayesianSubject)

module BayesianFilter =
  Machine (struct
      let vector = fv
    end)
    (struct
      type ranks = (string * float list) list

      let classify ranks =
        let subject_score = List.assoc BayesianSubject.name ranks |> List.hd in
        let body_score = List.assoc BayesianBody.name ranks |> List.hd in
        if subject_score > 0.7 && body_score > 0.7 then `Spam else `Ham
    end)
