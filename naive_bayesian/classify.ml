(* Here we extract the set of words present in an email but we might
      want to give it more weight by counting is proba multiple times.

   For exemple, a probability for the message
    "Multiple occurences of a word count as if the word only appears once"
   is computed as :

   p(multiple) * p(occurences) * p(word) * p(count) * p(only) * p(appears) * p(once)

   but could be :

   p(multiple) * p(occurences) * p(word) * p(count) * p(word) * p(only) * p(appears) * p(once)
*)

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
    (rare *. 0.5 +. float freq_in_all  *. spaminess)
    /. (rare +. float freq_in_all)
    |> normalize_proba

(* FROM SPAMORACLE *)
(* This is Robinson's chi-square stuff *)
let chi2_inverse m n =
  (* chi2 inverse of 2m with 2n degrees *)
  let t = ref (exp (-.m)) in
  let s = ref !t in
  for i = 1 to n do
    t := !t *. m /. float i;
    s := !s +. !t
  done;
  if !s >= 1.0 then 1.0 else !s

let log2 = log 2.0

let chi2_hypothesis ps =
  (* Compute -2 * ln (product ps).  Be careful with underflows. *)
  let p, pexp =
    List.fold_left
      (fun (p, pexp) ps ->
        (* product of all ps(i0 *)
        let p = p *. ps in
        let p, pexp =
          if p <= 1e-100 then
            (* to avoid overflows, if a value is to small, get the
               scientific value, and extract the exponential pexp*)
            let x, e = frexp p in
            (x, pexp + e)
          else (p, pexp)
        in
        (p, pexp))
      (1.0, 0) ps
  in
  chi2_inverse (-.(log p +. (log2 *. float pexp))) (List.length ps)

let _spaminess_score_graham proba =
  let p, pexp, cp, cpexp =
    List.fold_left
      (fun (p, pexp, cp, cpexp) ps ->
        let p = p *. ps in
        let p, pexp =
          if p <= 1e-100 then
            let m, e = frexp p in
            (m, pexp + e)
          else (p, pexp)
        in
        let cp = cp *. (1.0 -. ps) in
        let cp, cpexp =
          if cp <= 1e-100 then
            let m, e = frexp cp in
            (m, cpexp + e)
          else (cp, cpexp)
        in
        (p, pexp, cp, cpexp))
      (1.0, 0, 1.0, 0) proba
  in
  let p, cp =
    if cpexp < pexp then (p, ldexp cp (cpexp - pexp))
    else if cpexp > pexp then (ldexp p (pexp - cpexp), cp)
    else (p, cp)
  in
  p /. (p +. cp)

let compute_spam_probability (spaminess : (string * float) list) =
  let proba = List.map snd spaminess in
  let cproba = List.map (fun x -> 1.0 -. x) proba in
  let spam_proba1 =
    0.5 *. (1.0 +. chi2_hypothesis proba -. chi2_hypothesis cproba)
  in
  (* END OF SPAMORACLE *)
  let rec go acc = function
    | [] -> acc
    | p :: ps -> go (log (1. -. p) -. log p +. acc) ps
  in
  let _spam_proba2 = 1. /. (1. +. exp (go 0. proba)) in
  (*spaminess_score_graham proba*)
  spam_proba1

let spaminess db word =
  match Database.freq word db with
  | None -> failwith "todo"
  | Some Database.{ in_ham; in_spam } ->
      let spam_count, ham_count =
        (Database.spam_count db, Database.ham_count db)
      in      
      let res = compute_spaminess ~rare:3.0 spam_count ham_count in_spam in_ham
      in
      (*if word = "types" then
        begin
          let p_w_in_spam = float in_spam /. float spam_count in
          let p_w_in_ham = float in_ham /. float ham_count in  
          let spaminess = p_w_in_spam /. (p_w_in_spam +. p_w_in_ham) in
          Fmt.pr "in spam %f in ham %f spaminess %f res %f" p_w_in_spam p_w_in_ham spaminess res
        end;*) res
      

let _print_result (spaminess : (string * float) list) pspam =
  Fmt.pr "Used words:\n";
  List.iter
    (fun (w, spaminess) ->
      Fmt.pr "%s has a spaminess of %i\n" w (truncate (spaminess *. 100.0)))
    spaminess;
  Fmt.pr "Resulting probability of the mail to be a spam: %02f\n" pspam

let rank ~max_word bag_of_words db =
  let all_useful_words =
    Extract.WordSet.filter (fun w -> Database.mem w db) bag_of_words
  in
  (* probably more efficient to do it with a array of size [max_word]
     as it is done in spamoracle*)
  let sorted_spaminess =
    Extract.WordSet.fold
      (fun w acc -> (w, spaminess db w) :: acc)
      all_useful_words []
    |> List.sort (fun (_, spaminess1) (_, spaminess2) ->
           compare
             (abs_float (0.5 -. spaminess2))
             (abs_float (0.5 -. spaminess1)))
  in
  let rec keep_n_first n l =
    if n = 0 then []
    else
      match l with
      | [] -> []
      | x :: y ->
          x :: keep_n_first (n - 1) y
  in
  let retained_spaminess = keep_n_first max_word sorted_spaminess in
  let pspam = compute_spam_probability retained_spaminess in
  (*print_result retained_spaminess pspam;*)
  pspam
