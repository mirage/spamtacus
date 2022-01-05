open Cmdliner

exception Error of string

let run training_set_dir =
  let output = "mirage/lib/database/" |> Fpath.of_string |> Result.get_ok in
  let training_set =
    Spaml.
      {
        spam = Fpath.add_seg training_set_dir "spam";
        ham = Fpath.add_seg training_set_dir "ham";
      }
  in
  try
    Format.printf "Parsing and Writing database file@.";
    Spaml_mirage.train_and_write_to_file ~output training_set;
    Format.printf "Writing database.ml file@.";
    Spaml_mirage.serialize output "static_database";
    `Ok 0
  with Error str -> `Error (false, str)

let training_set_dir =
  let filename = Arg.conv (Fpath.of_string, Fpath.pp) in
  let doc =
    "The directory where the training set can be found. It must be divided \
     into two sub-directories $(i,spam) and $(i,ham)."
  in
  Arg.(
    required
    & pos ~rev:true 0 (some filename) None
    & info [] ~doc ~docv:"TRAINING_SET_PATH")
  
let cmd =
  let doc = "todo" in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) generate a $(i,spaml/mirage/database/static_database.ml).";
    ]
  in
  (Term.(ret (const run $ training_set_dir)), Term.info "generate" ~doc ~man)

let () = Term.(exit_status @@ eval cmd)
