let normalize_proba p = if p < 0.01 then 0.01 else if p > 0.99 then 0.99 else p

let compute_spaminess ?(rare = 3.0) spam_count ham_count freq_in_spam
    freq_in_ham =
  (* basic computation, hypothesis = ps = ph = 0.5 *)
  let p_w_in_spam = float freq_in_spam /. float spam_count in
  let p_w_in_ham = float freq_in_ham /. float ham_count in
  let spaminess = p_w_in_spam /. (p_w_in_spam +. p_w_in_ham) in
  if rare = 0.0 then normalize_proba spaminess
  else
    (* Rare words spaminess are corrected using the formula proved in Gary
       Robinson paper, « A statistical approach to the spam problem », *)
    let freq_in_all = freq_in_spam + freq_in_ham in
    ((rare *. 0.5) +. (float freq_in_all *. spaminess))
    /. (rare +. float freq_in_all)
    |> normalize_proba

let compute_spam_probability (spaminess : (string * float) list) =
  let proba = List.map snd spaminess in
  let rec go acc = function
    | [] -> acc
    | p :: ps -> go (log (1. -. p) -. log p +. acc) ps
  in
  let spam_proba2 = 1. /. (1. +. exp (go 0. proba)) in
  spam_proba2

let spaminess db word =
  match Database.freq word db with
  | None -> failwith "todo"
  | Some Database.{ in_ham; in_spam } ->
      let spam_count, ham_count =
        (Database.spam_count db, Database.ham_count db)
      in
      let res =
        compute_spaminess ~rare:3.0 spam_count ham_count in_spam in_ham
      in
      res

let rank ~max_word (bag_of_words : Extract.BagOfWords.t) db =
  (* extract number of occurence of each words in the db*)
  let all_useful_words =
    (* should be done when extracting *)
    Extract.BagOfWords.filter (fun w _ -> Database.mem w db) bag_of_words
  in
  let sorted_spaminess =
    (* compute spaminess *)
    Extract.BagOfWords.fold
      (fun w _occ acc -> (w, spaminess db w) :: acc)
      all_useful_words []
    (* and sort from higher to lower spaminess *)
    |> List.sort (fun (_, spaminess1) (_, spaminess2) ->
           compare
             (abs_float (0.5 -. spaminess2))
             (abs_float (0.5 -. spaminess1)))
  in
  let rec keep_n_first n l =
    if n = 0 then []
    else match l with [] -> [] | x :: y -> x :: keep_n_first (n - 1) y
  in
  (* keep the [max_word] with higher spaminess and compute probability
     of beeing a spam with it. *)
  let higher_spaminess = keep_n_first max_word sorted_spaminess in
  let pspam = compute_spam_probability higher_spaminess in
  pspam
