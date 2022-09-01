open Cmdliner

exception Error of string

let run training_set_dir =
  let output =
    "bayesian_filter/database/" |> Fpath.of_string |> Result.get_ok
  in
  let training_set =
    Spamtacus.
      {
        spam = Fpath.add_seg training_set_dir "spam";
        ham = Fpath.add_seg training_set_dir "ham";
      }
  in
  try
    Format.printf "Parsing and Writing database file@.";
    Spamtacus_bayesian.train_and_write_to_file ~output training_set;
    Format.printf "Writing database.ml file@.";
    Spamtacus_bayesian.serialize output "static_database";
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
      `P "$(tname) generate $(i,bayesian_filter/database/static_database.ml).";
    ]
  in
  Cmd.v
    (Cmd.info "generate" ~doc ~man)
    Term.(ret (const run $ training_set_dir))

let () = exit @@ Cmd.eval' cmd
