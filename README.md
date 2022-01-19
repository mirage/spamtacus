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
  val train : db -> [`Ham | `Spam] -> t -> db
  val write_db : out_channel -> db -> unit
  val read_db : in_channel option -> db
```
- and finally, an algorithm to rank an incoming mail from the extracted parts and a pre-computed database.
```ocaml
  val rank : t -> db -> rank
```

## Filter implementation

As explained above, to implement a filter, one needs to provide two things: some features and a
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

## Filter performances

We have built a simple performance test to evaluate how good (or bad!) our
bayesian filter is at filtering spams. The two important numbers we
will be looking at are:
- the percentage of hams labelled as spam (i.e. false positives)
- the percentage of spams labelled as ham (i.e. false negatives). 

### How to launch the tests
The performances tests can be launch with the command:
```sh
$ dune exec ./test/test_stream.exe [testing_corpus_directory]
```

It prints in return all the important statistics like the number of
false negatives and false positives.

### Performances of our bayesian spam filter

We used mail corpus available
[here](https://spamassassin.apache.org/old/publiccorpus/readme.html). Mails
are sorted in three categories: easy ham, hard ham and spam. To
evaluate our filter, we built several training sets with:
- various sizes,
- different proportions of spams versus hams,
- with ou without hard hams in it.

Then we computed statistics by testing the resulting filters on two
mails corpus: once with easy hams only and another with hard hams. No
mails present in the testing corpus are in a training set. In a
nutshell here are the important results:
- a **balanced** training set (same numbers of spam and ham) results in
a filter that is very good at filtering spams (< 2% of spams are not filtered) but also tends
  to filter a lot of hams (> 15% even with a large training set).  
- a training set with **many more hams than spams** create a filter that almost never
  blocks hams (< 1%) but lets around 12% of spam go though. In
  our opinion, it is the best compromise.
- **hard hams** are ... hard to not filter. If they are added to the
  training set, they weaken the filter that lets more than 40% of
  spams go though.

To conclude, as expected, we most certainly need to improve our filter
by implementing new features. Improved features could for example
take into account html elements, other headers, other languages etc..

#### Important notes

Only the first feature (naive bayesian filter on body content)
actually has an impact on the results below. No forbidden attachments
are present in the corpus and the filter on subject header returns
unconclusive probabilities and need to be improved.

No intern feature parameters have been changed for theses tests nor the
[classify] function of the filter. 

## State of the art
See [here](soa.md).

## Fundings
`Spamtacus` has received funding from the Next Generation Internet Initiative
(NGI) within the framework of the DAPSI Project.
