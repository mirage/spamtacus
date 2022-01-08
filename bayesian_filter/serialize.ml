let ocamlify ?(uncapitalize = true) s =
  let s = if uncapitalize then String.uncapitalize_ascii s else s in
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | '-' | '.' -> Buffer.add_char b '_'
      | _ -> ())
    s;
  let s' = Buffer.contents b in
  if String.length s' = 0 || ('0' <= s'.[0] && s'.[0] <= '9') then
    raise (Invalid_argument s);
  s'

let reserved_keyword = [ ("virtual", "virtual'") ]

let ocamlify ?(uncapitalize = true) s =
  match List.assoc s reserved_keyword with
  | s' -> s'
  | exception Not_found -> ocamlify ~uncapitalize s

exception End

let build_filename filename idx =
  Format.asprintf "%s_%03d.ml" (ocamlify filename) idx

let build_modulename filename idx =
  Format.asprintf "%s_%03d" (String.capitalize_ascii (ocamlify filename)) idx

let serialize_freqs_into_multiple_files output_dir name freqs =
  let open Database in
  let rfreqs = ref (Some freqs) in

  let serialize ppf freqs =
    let rec go n freqs =
      match (n, freqs) with
      | _, [] ->
          rfreqs := None;
          ()
      | 0, _ ->
          rfreqs := Some freqs;
          ()
      | m, (w, { in_spam; in_ham }) :: xs ->
          Format.fprintf ppf "@[|> Map.add %S { in_spam=%i; in_ham=%i}@]@." w
            in_spam in_ham;
          go (m - 1) xs
    in
    go 1000 freqs
  in

  let print_one_file ~first idx freqs =
    let filename = build_filename name idx in
    let modulename = build_modulename name (idx - 1) in
    let oc = open_out (Fpath.add_seg output_dir filename |> Fpath.to_string) in
    let ppf = Format.formatter_of_out_channel oc in
    Format.fprintf ppf "open Database@.";
    if first then
      Format.fprintf ppf "let db = Map.empty @,@[<4>%a@]\n%!" serialize freqs
    else (
      Format.fprintf ppf "open %s@." modulename;
      Format.fprintf ppf "let db = db @,@[<4>%a@]\n%!" serialize freqs);
    close_out oc
  in

  let rec run idx freqs =
    print_one_file ~first:false idx freqs;
    match !rfreqs with None -> idx | Some freqs -> run (idx + 1) freqs
  in
  print_one_file ~first:true 0 freqs;
  match !rfreqs with None -> 0 | Some freqs -> run 1 freqs

let serialize_db freqs_name ppf db =
  let open Database in
  Format.fprintf ppf "{ nb_spam = %i; nb_ham = %i; freqs = %s.db}" db.nb_spam
    db.nb_ham
    (String.capitalize_ascii freqs_name)

(* TODO : sorting filtering the db could be done before writing it *)
let serialize oc output_dir name db =
  let ppf = Format.formatter_of_out_channel oc in

  let db =
    (* filter the db to keep only the useful words (that
         appears enough times in the training set) *)
    Database.
      {
        db with
        freqs =
          Map.filter
            (fun _ { in_spam; in_ham } -> in_spam + (2 * in_ham) >= 5)
            db.freqs;
      }
  in
  let name = ocamlify ~uncapitalize:true name in
  let freqs = Database.Map.bindings db.freqs in
  let idx = serialize_freqs_into_multiple_files output_dir name freqs in
  let freqs_name = build_modulename name idx in
  Format.fprintf ppf "let %s = Database.@[<2>%a@]@.%!" name
    (serialize_db freqs_name) db
