# Spamtacus: a customizable mail filter

`Spamtacus` provides tools to build a customizable mail filter. It
contains three librairies:

- (1) A [filter abstraction](https://github.com/lyrm/spamtacus/tree/main/lib) based on [supervized learning
model](https://en.wikipedia.org/wiki/Supervised_learning),
- (2) An [implementation](https://github.com/lyrm/spamtacus/tree/main/bayesian_filter) of this abstraction composed of a naive bayesian algorithm and
an attachment checker that can be seen as a basic antivirus,
- (3) A [specific application](https://github.com/lyrm/spamtacus/tree/main/mirage) for [`MirageOS
unikernel`](https://github.com/dinosaure/ptt/tree/master/unikernel/spamfilter)
with serialization of the database to a static value.

## Filter abstraction or how to build your own spam filter

We have built our [filter
abstraction](https://github.com/lyrm/spamtacus/tree/main/lib) on the
supervized learning model, meaning a mail is labelled based on
examples: a static set of pre-labelled mails -called a training set-
is used to train a machine learning algorithm. This training is
basically converted into a database and for every incoming mail, the
algorithm extracts the pertinent parts of the mail, compares it to the
database (its examples) and ranks the mail.

The *pertinent parts* extracted from an incoming mail are called
features. To build a filter, features have to be provided as well as a
`classify` function -basically a decision tree that translates ranks
computed for every feature into a label.

```ocaml
module type FILTER = functor (Features : FV) (DecisionTree : DT)
```

Each feature is defined by:
- an unique identifier
```ocaml
  val name: string
```

- what to extract from incoming mail chunks and headers and how to extract it
```ocaml
  type t
  (** Type of extracted mail parts. *)
  val empty : t
  val partial_extract : Mrmime.Header.t -> string -> string list
  (** How to extract it from a mail chunk ... *)
  val extract_from_header_tree : Mrmime.Header.t * unit Mrmime.Mail.t -> t
  (** ... or from mail headers.*)
  val add_partial : t -> string list -> t
  (** How to convert the extracted strings into a feature. *)
```

- a description of this specific feature database and how to train, read and write it
```ocaml
  type db
  val empty_db : db
  val train : db -> label -> t -> db
  val write_db : out_channel -> db -> unit
  val read_db : in_channel option -> db
```
- and finally, an algorithm to rank an incoming mail from the extracted parts and a pre-computed database.
```ocaml
  val rank : t -> db -> rank
```

## Filter implementation

As explained above, to implement a filter, one needs to provide two thinks: some features and a
decision tree. Our [provided implementation](https://github.com/lyrm/spamtacus/tree/main/bayesian_filter) contains three features:
- a naive Bayesian filter on mail body content
- a naive Bayesian filter on values of `subject` headers
- a basic antivirus that rejects mails with forbidden attachment (`.exe` files for example)

The decision tree is pretty simple:
- A mail is labelled as `Spam` if it contains a forbidden attachment or if the probability of beeing a spam computed by any of the two bayesian filters is greater than 0.7.
- If no forbidden attachment has been found and both probabilities of beeing a spam are lower than 0.3, it is labelled as `Ham`.
- Otherwise an `Unknown` label is returned.
 
## Application for MirageOS

One of the primary purpose of this library is to provide every tools
necessary to build a spam filter for a MirageOs unikernel. For that,
we add a few things to our spam filter implementation. We need:
- to be able to serialize the database into a static value, as
MirageOS does not provide a file system.
- to provide a function that ranks an incoming stream mail and
  returns it appended with proper spam header.

Serialization, as well as training, as to be done in a pre-deployment
phase using the command below:
```sh
$ dune exec ./bin/generate.exe [training_set_directory]
```
A pre-computed database is already provided. It had been computed with
a publicly available mail database ([SpamAssasin's public
corpus](https://spamassassin.apache.org/old/publiccorpus/readme.html)).

Finally, a performance test for our spam filter can be done with the command 
```sh
$ dune exec ./test/test.exe
```
It returns:
- how many hams and spams habe been properly labelled (resp. true negative and true positive),
- how many hams have been labelled as spam (false positive),
- how many spamls have been labelled as ham (false negative).

## State of the art
See [here](soa.md).
## Fundings
`Spamtacus` has received funding from the Next Generation Internet Initiative
(NGI) within the framework of the DAPSI Project.
