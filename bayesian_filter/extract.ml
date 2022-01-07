(* Parser*)
open Angstrom

type t = Word of string | Num of string | Upper of string

(* [lowercase s] maps uppercase in [s] to lowercase. *)
let lowercase s = String.lowercase_ascii s

(* [all_uppercase s] checks if a string is all uppercase characters *)
let is_all_uppercase s =
  try
    String.iter (function 'A' .. 'Z' -> () | _ -> raise Exit) s;
    true
  with Exit -> false

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_other s = (not (is_letter s)) && not (is_digit s)

(* [numeric] is defined are a string of digit or dot, comma, dollar,
   euro or percent char. *)
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

(* Idea of improvement : add the possibility of choosing the way a
   string is parsed with a [angstrom.t] as input. *)
let t_list = other *> many (numeric <|> word <* other) <* end_of_input

(* [split string] splits [string] into [t] elements (word, numeric
   string or uppercased words) *)
let split text =
  match Angstrom.parse_string ~consume:Consume.All t_list text with
  | Ok fs -> fs
  | Error m -> failwith m

module WordSet = Set.Make (struct
  type t = string

  let compare = compare
end)

(* Kept words : between 3 and 12 letters
   Kept numeric : between 3 and 8 characters
*)
let filter = function
  | Word w | Upper w ->
      let len = String.length w in
      len >= 3 && len <= 12
  | Num n ->
      let len = String.length n in
      len >= 3 && len <= 8

(* [add wordset word] adds a [word] to a [wordset] with an exception:
   uppercased words are added twice: lowercased and uppercased. *)
let add words = function
  | Word w | Num w -> WordSet.add w words
  | Upper w -> WordSet.add w words |> WordSet.add (lowercase w)

(* [extract str] splits [str] into words, all-uppercased words and
   numeric strings, filters it depending on their length and returns
   the list of extracted unique words (uppercased or not) and numeric
   strings. *)
let extract str =
  let valid_wordset = split str in
  List.fold_left
    (fun wordset feature ->
      if filter feature then add wordset feature else wordset)
    WordSet.empty valid_wordset

let extract_list str = extract str |> WordSet.elements
