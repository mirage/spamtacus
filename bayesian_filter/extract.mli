module BagOfWords : sig
  type t

  val empty : t
  val add : string -> t -> t
  val fold : (string -> int -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (string -> int -> bool) -> t -> t
  val union : t -> t -> t
end

val extract : string -> BagOfWords.t
(** [extract str] splits [str] into words, all-uppercased words and
    numeric strings, filters it depending on their length and returns the
    extracted words (uppercased or not) and numeric strings associated to their
    frequency in the input string. *)

val extract_list : string -> string list
(** [extract_list str] returns all unique words in [str]. *)
