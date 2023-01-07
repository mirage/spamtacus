val rank : max_word:int -> Extract.BagOfWords.t -> Database.db -> float
(** [rank ~max_word bag_of_words db] returns the probability of beeing
    a spam from a [bag_of_words] extracted from a mail, using a naive bayesian
    approach with a precomputed database [db].

    [max_word] determines how many words of [bag_of_words] can be used to
    compute this probability. The kept words are the one with the higher
    spaminess (meaning they appear in a lot in spams and not or almost never in
    hams).

    See {!module:Extract} module for functions to create a bag of word from an
    incoming string.
*)
