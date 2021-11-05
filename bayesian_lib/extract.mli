module WordSet : Set.S with type elt = string

type mail = Mrmime.Header.t * string Mrmime.Mail.t
type extractor = mail -> WordSet.t

val extract_bodies_words : extractor
val extract_main_subject_values : extractor
