module WordSet : Set.S with type elt = string

val extract : string -> WordSet.t
(** [extract str] splits [str] into words, all-uppercased words and
   numeric strings, filters it depending on their length and returns
   the set of extracted words (uppercased or not) and numeric
   strings. *)

val extract_list : string -> string list
(** [extract_list str] works like [extract] but returns a list
   instead. *)
