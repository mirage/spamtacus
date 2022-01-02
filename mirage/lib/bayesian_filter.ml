open Filter_imp
type training_set = Spaml.training_set

let serialize db_dir filename =
  let static_value_filename =
    Fpath.add_seg db_dir filename |> Fpath.add_ext "ml"
  in
  let oc = open_out (Fpath.to_string static_value_filename) in

  let serialize name (db : Bayesian.Database.db) =
    Serialize.serialize oc db_dir name db
  in
  BayesianBody.(
    let ic = open_in Fpath.(add_seg db_dir name |> to_string) in
    let db = Bayesian.Database.read ic in
    close_in ic;
    serialize name db);
  BayesianSubject.(
    let ic = open_in Fpath.(add_seg db_dir name |> to_string) in
    let db = Bayesian.Database.read ic in
    close_in ic;
    serialize name db);
  close_out oc

let rank (input : unit -> (string * int * int) option Lwt.t) = Stream.rank input

let train_and_write_to_file = Filter_imp.train_and_write_to_file
