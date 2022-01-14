(* Database read and write functions.*)

(* Some note for future improvements *)
(* 1. About words frequency:

   Spamoracle actually separates words in two categories: frequent
   ones and rare ones. A full database (containing both categories) is
   used when building or adding to the database. When testing an mail
   to classify it, only the frequent words are read and used.

   In our current implementation for mirage, the filtering of frequent
   words is done when writing the static database: only the [n] more
   frequent in mails (spams and hams) are actually written. *)

(* 2. About corrected probabilty:

   Here, we are actually using a corrected probability that takes into
   account the frequency of a word.  It requires however than the
   probability of a mail to be a spam is 0.5, meaning the number of
   spams in the database must be equal to the number of hams. *)

module Map = Map.Make (struct
  type t = string

  let compare = compare
end)

type db = { nb_spam : int; nb_ham : int; freqs : freq Map.t }

and freq = { in_spam : int; in_ham : int }

let create () : db = { nb_spam = 0; nb_ham = 0; freqs = Map.empty }
let mem w db = Map.mem w db.freqs

(* Database file format : first line contain nb of spams and nb of ham
   then each line contains a word and the number of spams in which it
   appears and the number of hams in which it appears. *)
(* [nb_spam] [nb_ham]
   [w0] [freq_of_w0_in_spam] [freq_of_w0_in_ham]
   ...
   [wn] [freq_of_wn_in_spam] [freq_of_wn_in_ham] *)
let parse_first_line line =
  match String.split_on_char ' ' line with
  | [ nb_spam; nb_ham ] ->
      let nb_spam, nb_ham = (int_of_string nb_spam, int_of_string nb_ham) in
      let freqs = Map.empty in
      { nb_spam; nb_ham; freqs }
  | _ -> failwith "todo database parse_first_line"

let parse_line db line =
  match String.split_on_char ' ' line with
  | [ word; in_spam; in_ham ] ->
      let freq =
        { in_spam = int_of_string in_spam; in_ham = int_of_string in_ham }
      in
      { db with freqs = Map.add word freq db.freqs }
  | _ -> failwith "todo database parse_line"

let read ic =
  let first_line = input_line ic in
  let db = parse_first_line first_line in
  let rec go db =
    match input_line ic with
    | line -> go (parse_line db line)
    | exception End_of_file -> db
  in
  let db = go db in
  db

let write oc db =
  Printf.fprintf oc "%d %d\n" db.nb_spam db.nb_ham;
  Map.iter
    (fun w freq -> Printf.fprintf oc "%s %d %d\n" w freq.in_spam freq.in_ham)
    db.freqs

let init_freq_with i = function
  | `Ham -> { in_spam = 0; in_ham = i }
  | `Spam -> { in_spam = i; in_ham = 0 }

let add_to_freq i label { in_spam; in_ham } =
  match label with
  | `Ham -> { in_spam; in_ham = in_ham + i }
  | `Spam -> { in_spam = in_spam + i; in_ham }

(* [add_word label w db] add +1 to the proper frequency (ham or spam)
   of a word [w] in [db]. *)
let add_word (label : [ `Spam | `Ham ]) (w, occ) db =
  match Map.find_opt w db.freqs with
  | None -> { db with freqs = Map.add w (init_freq_with occ label) db.freqs }
  | Some freq ->
      { db with freqs = Map.add w (add_to_freq occ label freq) db.freqs }

(* [add label mail db] *)
let add label (bow : Extract.BagOfWords.t) (db : db) =
  let db =
    match label with
    | `Spam -> { db with nb_spam = db.nb_spam + 1 }
    | `Ham -> { db with nb_ham = db.nb_ham + 1 }
  in
  Extract.BagOfWords.fold (fun w occ db -> add_word label (w, occ) db) bow db

(* [add_spam mail db] cut a mail into elementary pieces and *)
let add_spam = add `Spam
let add_ham = add `Ham

(* Accessors *)
let spam_count db = db.nb_spam
let ham_count db = db.nb_ham
let freq word db = Map.find_opt word db.freqs

(* Debug functions *)
let add_spaces w n =
  let len = String.length w in
  let spaces = String.make (max 0 (n - len)) ' ' in
  w ^ spaces

let print ?(min_freq = 1) ({ nb_spam; nb_ham; freqs } : db) =
  let freqs = Map.bindings freqs in
  Fmt.pr " Number of spams: %d\n" nb_spam;
  Fmt.pr "  Number of hams: %d\n" nb_ham;
  Fmt.pr "           Total: %d\n\n" (nb_ham + nb_spam);
  List.iter
    (fun (w, f) ->
      if f.in_spam >= min_freq || f.in_ham >= min_freq then
        Fmt.pr "%s appears in %d hams and in %d spams\n" (add_spaces w 30)
          f.in_ham f.in_spam)
    freqs;
  Fmt.pr "%!"
