(* Database read and write functions.

   Spamoracle actually separates words in two categories: frequent
   ones and rare ones. A full database (containing both categories) is
   used when building or adding to the database. When testing an
   email, only the frequent words are read and used.

   Here, we are actually using a corrected probability that take into
   account the frequency of a word (should it be in how many mails the
   word appeared or its actual occurences across all mails ?).

   It requires however than the probability of an mail to be a spam is
   0.5, meaning the number of spams in the database must be equal to
   the number of hams (TODO: find a paper or some references about
   that). *)
open Mrmime

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

let init_freq = function
  | `Ham -> { in_spam = 0; in_ham = 1 }
  | `Spam -> { in_spam = 1; in_ham = 0 }

let incr_freq label { in_spam; in_ham } =
  match label with
  | `Ham -> { in_spam; in_ham = in_ham + 1 }
  | `Spam -> { in_spam = in_spam + 1; in_ham }

(** [add_word label w db] add +1 to the proper frequency (ham or
   spam) of a word [w] in [db]. *)
let add_word (label : [ `Spam | `Ham ]) w db =
  match Map.find_opt w db.freqs with
  | None -> { db with freqs = Map.add w (init_freq label) db.freqs }
  | Some freq -> { db with freqs = Map.add w (incr_freq label freq) db.freqs }

(** [add label mail db*)
let add label (mail : Header.t * string Mail.t) extract (db : db) =
  let db =
    match label with
    | `Spam -> { db with nb_spam = db.nb_spam + 1 }
    | `Ham -> { db with nb_ham = db.nb_ham + 1 }
  in
  let words = extract mail in
  Extract.WordSet.fold (add_word label) words db

(** [add_spam mail db] cut a mail into elementary pieces and *)
let add_spam = add `Spam

let add_ham = add `Ham

(* Accessors *)
let spam_count db = db.nb_spam
let ham_count db = db.nb_ham
let freq word db = Map.find_opt word db.freqs

(** Debug functions *)
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
(*
(** Function to encode/decode database to/from json *)
let encoding_db : db Json_encoding.encoding =
  let open Json_encoding in
  let nb_spam = req "nb_spam" int in
  let nb_ham = req "nb_ham" int in
  let freq =
    let word = req "word" string in
    let in_spam = req "in_spam" int in
    let in_ham = req "in_ham" int in
    obj3 word in_spam in_ham
  in
  let freqs = req "freqs" (list freq) in
  conv
    (fun db ->
      let freqs_l =
        Map.fold
          (fun w { in_spam; in_ham } acc -> (w, in_spam, in_ham) :: acc)
          db.freqs []
      in
      (db.nb_spam, db.nb_ham, freqs_l))
    (fun (nb_spam, nb_ham, freqs_l) ->
      let freqs =
        List.fold_left
          (fun map (word, in_spam, in_ham) ->
            Map.add word { in_spam; in_ham } map)
          Map.empty freqs_l
      in
      { nb_spam; nb_ham; freqs })
    (obj3 nb_spam nb_ham freqs)

type value = [ `Null | `Bool of bool | `String of string | `Float of float ]

let flat json : Jsonm.lexeme list =
  let rec arr acc k = function
    | [] -> k (List.rev (`Ae :: acc))
    | (#value as x) :: r -> arr (x :: acc) k r
    | `A l :: r -> arr [ `As ] (fun l -> arr (List.rev_append l acc) k r) l
    | `O l :: r -> obj [ `Os ] (fun l -> arr (List.rev_append l acc) k r) l
  and obj acc k = function
    | [] -> k (List.rev (`Oe :: acc))
    | (n, x) :: r ->
        base (fun v -> obj (List.rev_append v (`Name n :: acc)) k r) x
  and base k = function
    | `A l -> arr [ `As ] k l
    | `O l -> obj [ `Os ] k l
    | #value as x -> k [ x ]
  in
  base (fun l -> l) json

type unit_or_error = (unit, [ `Msg of string ]) result
type buffer = bytes * int * int
type transmit = buffer -> buffer

(** three possibles destinations :
- Manual transmit : a is if type buffer in this case 
- Buffer buf : a is a buffer writing function of type Buffer.t -> unit_or_error
- Channel oc : a is a write function of type (out_channel -> unit_or_error) 
*)
type 'a dst =
  | Manual : transmit -> buffer dst
  | Buffer : Buffer.t -> (Buffer.t -> unit_or_error) dst
  | Channel : out_channel -> (out_channel -> unit_or_error) dst

let to_json : type a. dst:a dst -> a -> db -> unit_or_error =
 fun ~dst a db ->
  let to_dst : type a. a dst -> Jsonm.dst = function
    | Manual _ -> `Manual
    | Buffer buffer -> `Buffer buffer
    | Channel oc -> `Channel oc
  in
  let encoder = Jsonm.encoder ~minify:true (to_dst dst) in
  let buf, off, len =
    match dst with
    | Manual _ ->
        let buf, off, len = a in
        (ref buf, ref off, ref len)
    | Buffer _ -> (ref Bytes.empty, ref 0, ref 0)
    | Channel _ -> (ref Bytes.empty, ref 0, ref 0)
  in
  let go json =
    let flat = flat json in
    List.iter
      (fun lexeme ->
        match Jsonm.encode encoder (`Lexeme lexeme) with
        | `Ok -> ()
        | `Partial -> (
            match dst with
            | Manual transmit ->
                let buf', off', len' =
                  transmit (!buf, !off, !len - Jsonm.Manual.dst_rem encoder)
                in
                buf := buf';
                off := off';
                len := len';
                Jsonm.Manual.dst encoder buf' off' len'
            | Buffer _ -> ()
            | Channel _ -> ()))
      flat;

    let rec go : type a. a dst -> a -> unit_or_error =
     fun dst a ->
      match (Jsonm.encode encoder `End, dst) with
      | `Ok, Buffer buf -> a buf
      | `Ok, Channel oc -> a oc
      | `Ok, Manual _ -> Ok ()
      | `Partial, Manual transmit ->
          let buf', off', len' =
            transmit (!buf, !off, !len - Jsonm.Manual.dst_rem encoder)
          in
          buf := buf';
          off := off';
          len := len';
          Jsonm.Manual.dst encoder buf' off' len';
          go dst a
      | `Partial, Buffer _ -> assert false
      | `Partial, Channel _ -> assert false
    in
    go dst a
  in
  go (Json_encoding.construct encoding_db db)


(* *)
*)
