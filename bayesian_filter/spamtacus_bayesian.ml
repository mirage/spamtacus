(**
 Implementation of a bayesian filter applied to two features :

  - the main subject of the mail (meaning only the first headers
   are introspect to extract a subject)
   
 - all bodies included in the mail.   
*)
include Spamtacus
module BayesianBody = Features.BayesianBody
module BayesianSubject = Features.BayesianSubject
module BasicAntiVirus = Features.BasicAntiVirus

let fv =
  create_fv ()
  |> add_feature (module BayesianBody)
  |> add_feature (module BayesianSubject)
  |> add_feature (module BasicAntiVirus)

module BayesianFilter =
  Filter (struct
      let vector = fv
    end)
    (struct
      let classify ranks =
        let antivirus_score = List.assoc BasicAntiVirus.name ranks |> List.hd in  
        let subject_score = List.assoc BayesianSubject.name ranks |> List.hd in
        let body_score = List.assoc BayesianBody.name ranks |> List.hd in
        if antivirus_score = 1.0 then `Spam else
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
