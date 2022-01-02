(* Todo

    1. Define what a feature is (right now, a feature = any word of
    length > 2).  Features can be defined as words, segments of words,
    words of at least a minimun length, or any other element of a
    document (like a html element, a particular header etc..)

   2. Right now, a mail is converted to a set of words (so we can know
    if a word appeared in a mail), but it can be useful to actually
    know the number of occurences of a word in a given mail.
*)

(* Parser*)
open Angstrom

type t = Word of string | Num of string | Upper of string

(** [lowercase s] maps uppercase in [s] to lowercase. *)
let lowercase s = String.lowercase_ascii s

(** [all_uppercase s] checks if a string is all uppercase characters *)
let is_all_uppercase s =
  try
    String.iter (function 'A' .. 'Z' -> () | _ -> raise Exit) s;
    true
  with Exit -> false

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_other s = (not (is_letter s)) && not (is_digit s)

let numeric =
  let is_numeric = function
    | '.' | ',' | '$' | '%' | '\164' -> true
    | s -> is_digit s
  in
  take_while1 is_numeric >>| fun n -> Num n

let word =
  take_while1 (function '\'' | '-' -> true | s -> is_letter s) >>| fun w ->
  if is_all_uppercase w then Upper w else Word (lowercase w)

let other = skip_while is_other

(* Ajouter modularité pour pouvoir mettre des angstrom.t personnalisés
   en entrée (dépendant du langage ? prenant une liste de pattern ? *)
let features = other *> many (numeric <|> word <* other) <* end_of_input

let split text =
  match Angstrom.parse_string ~consume:Consume.All features text with
  | Ok fs -> fs
  | Error m -> failwith m

(* Extract functions *)
module WordSet = Set.Make (struct
  type t = string

  let compare = compare
end)

let filter = function
  | Word w | Upper w ->
      let len = String.length w in
      len >= 3 && len <= 12
  | Num n ->
      let len = String.length n in
      len >= 3 && len <= 8

let add words = function
  | Word w | Num w -> WordSet.add w words
  | Upper w -> WordSet.add w words |> WordSet.add (lowercase w)

let extract str =
  let valid_wordset = split str in
  List.fold_left
    (fun wordset feature ->
      if filter feature then add wordset feature else wordset)
    WordSet.empty valid_wordset

let extract_list str =
  let valid_wordset = split str in
  List.fold_left
    (fun wordset feature ->
      if filter feature then add wordset feature else wordset)
    WordSet.empty valid_wordset |> WordSet.elements

(*let extract_bodies_words (mail : Mrmime.Header.t * string Mrmime.Mail.t) =
    let rec go wordset = function
      | _, Mrmime.Mail.(Leaf body) ->
          let valid_wordset = split body in
          List.fold_left
            (fun wordset feature ->
              if filter feature then add wordset feature else wordset)
            wordset valid_wordset
      | _, Message (h, m) -> go wordset (h, m)
      | _, Multipart parts ->
          List.fold_left
            (fun wordset (h, m) ->
              match m with Some m -> go wordset (h, m) | None -> failwith "")
            wordset parts
    in
    go WordSet.empty mail

  let extract_main_subject_values (header, _) =
    let all_subjet_values =
      Mrmime.Header.assoc Mrmime.Field_name.subject header
      |> List.map (fun h -> Prettym.to_string Mrmime.Field.Encoder.field h)
    in
    let valid_wordset = List.map split all_subjet_values |> List.concat in
    List.fold_left
      (fun wordset feature ->
        if filter feature then add wordset feature else wordset)
      WordSet.empty valid_wordset
*)
