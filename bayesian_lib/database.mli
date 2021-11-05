(*
    Database build, read and write functions.
*)
open Mrmime
module Map : Map.S with type key = string

type db
(** Describe a database *)

and freq = { in_spam : int; in_ham : int }
(** Described in how many spams and hams a given word appeared *)

val read : in_channel -> db
(** [read filename]*)

val write : out_channel -> db -> unit
(** [write filename db]*)

type unit_or_error = (unit, [ `Msg of string ]) result
type buffer = bytes * int * int
type transmit = buffer -> buffer

type 'a dst =
  | Manual : transmit -> buffer dst
  | Buffer : Buffer.t -> (Buffer.t -> unit_or_error) dst
  | Channel : out_channel -> (out_channel -> unit_or_error) dst

val to_json : dst:'a dst -> 'a -> db -> unit_or_error

val create : unit -> db
(** [create ()] *)

val mem : string -> db -> bool
(** [mem word db] *)

val add_spam : Header.t * string Mail.t -> Extract.extractor -> db -> db
(** [add_spam spam db] *)

val add_ham : Header.t * string Mail.t -> Extract.extractor -> db -> db
(** [add_ham ham db] *)

(* Accessors *)
val spam_count : db -> int
val ham_count : db -> int
val freq : string -> db -> freq option

(* Debug *)
val print : ?min_freq:int -> db -> unit
