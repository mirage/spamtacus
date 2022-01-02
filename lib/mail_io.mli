exception ParsingError of string

val parse : Fpath.t -> Mrmime.Header.t * string Mrmime.Mail.t
