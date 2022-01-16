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

module BagOfWords = struct
  module Map = Map.Make (struct
    type t = string

    let compare = compare
  end)

  type t = int Map.t

  let empty = Map.empty

  let add w (bow : t) : t =
    match Map.find_opt w bow with
    | None -> Map.add w 1 bow
    | Some i -> Map.add w (i + 1) bow

  let fold (f : string -> int -> 'a -> 'a) (t : t) (acc : 'a) : 'a =
    Map.fold f t acc

  let filter (f : string -> int -> bool) (t : t) : 'a = Map.filter f t
  let union m m' = Map.union (fun _ occ occ' -> Some (occ + occ')) m m'
end

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

(* [add bow word] adds a [word] to a bag of words [bow] with an exception:
   uppercased words are added twice: lowercased and uppercased. *)
let add bow = function
  | Word w | Num w -> BagOfWords.add w bow
  | Upper w -> BagOfWords.add w bow |> BagOfWords.add (lowercase w)

(* [extract str] splits [str] into words, all-uppercased words and
   numeric strings, filters it depending on their length and returns
   the list of extracted words (uppercased or not) and numeric
   strings. *)
let extract str =
  List.fold_left
    (fun bow word -> if filter word then add bow word else bow)
    BagOfWords.empty (split str)

let extract_list str =
  let bow = extract str in
  BagOfWords.fold (fun w _ seq -> w :: seq) bow []
