(** Database build, read and write functions. *)

module Map : Map.S with type key = string

type db = { nb_spam : int; nb_ham : int; freqs : freq Map.t }
(** Describe a database *)

and freq = { in_spam : int; in_ham : int }
(** Described in how many spams and hams a given word appeared *)

val read : in_channel -> db
(** [read filename]*)

val write : out_channel -> db -> unit
(** [write filename db]*)

val create : unit -> db
(** [create ()] *)

val mem : string -> db -> bool
(** [mem word db] *)

val add_spam : Extract.BagOfWords.t -> db -> db
(** [add_spam spam db] *)

val add_ham : Extract.BagOfWords.t -> db -> db
(** [add_ham ham db] *)

(* Accessors *)
val spam_count : db -> int
val ham_count : db -> int
val freq : string -> db -> freq option

(* Debug *)
val print : ?min_freq:int -> db -> unit
