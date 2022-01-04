module WordSet : Set.S with type elt = string
    
val extract : string -> WordSet.t
val extract_list : string -> string list
