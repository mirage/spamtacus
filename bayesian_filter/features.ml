include Spamtacus

module BayesianBody : FEATURE with type db = Database.db = struct
  let name = "BayesianBody"

  type t = Extract.BagOfWords.t

  let empty = Extract.BagOfWords.empty

  type db = Database.db

  let empty_db = Database.create ()

  let partial_extract _ (str : string) : string list =
    let extracted =
      Extract.extract str |> fun e ->
      Extract.BagOfWords.fold (fun w _ acc -> w :: acc) e []
    in
    extracted

  let extract_from_header_tree _header_tree : t = Extract.BagOfWords.empty

  let add_partial (t : t) extracted : t =
    List.fold_left (fun t word -> Extract.BagOfWords.add word t) t extracted

  let write_db oc db = Database.write oc db
  let read_db _ic = Static_database.bayesianBody

  let train db label bow : db =
    match label with
    | `Spam -> Database.add_spam bow db
    | `Ham -> Database.add_ham bow db

  let rank t db = [ Classify.rank ~max_word:20 t db ]
end

module BayesianSubject : FEATURE with type db = Database.db = struct
  let name = "BayesianSubject"

  type t = Extract.BagOfWords.t

  let empty = Extract.BagOfWords.empty

  type db = Database.db

  let empty_db = Database.create ()

  let extract_main_subject_value header =
    Mrmime.Header.assoc Mrmime.Field_name.subject header
    |> List.map (fun h -> Prettym.to_string Mrmime.Field.Encoder.field h)

  let partial_extract _ _ = []

  let extract_from_header_tree (header, tree) : t =
    let extract_and_add acc header =
      let main_subjects = extract_main_subject_value header in
      List.fold_left
        (fun bow subject ->
          let new_bow = Extract.extract subject in
          Extract.BagOfWords.union bow new_bow)
        acc main_subjects
    in
    let rec go acc = function
      | Mrmime.Mail.Leaf () -> acc
      | Message (h, t) ->
          let acc = extract_and_add acc h in
          go acc t
      | Multipart parts ->
          List.fold_left
            (fun acc (header, bodyopt) ->
              let acc = extract_and_add acc header in
              match bodyopt with None -> acc | Some t -> go acc t)
            acc parts
    in
    go (extract_and_add Extract.BagOfWords.empty header) tree

  let add_partial (t : t) (extracted : string list) : t =
    List.fold_left (fun t word -> Extract.BagOfWords.add word t) t extracted

  let write_db oc db = Database.write oc db
  let read_db _ic = Static_database.bayesianSubject

  let train db label t : db =
    match label with
    | `Spam -> Database.add_spam t db
    | `Ham -> Database.add_ham t db

  let rank t db = [ Classify.rank ~max_word:1 t db ]
end

module BasicAntiVirus : FEATURE = struct
  let name = "BasicAntiVirus"

  type t = bool

  let empty = false

  type db = unit

  let empty_db = ()

  let rec list_hd_map_or ~f = function
    | [] -> None
    | x :: r -> (
        match f x with Some x -> Some x | None -> list_hd_map_or ~f r)

  (* Extract the parameter [name] from [content-type] header if existed
     and returns its value. *)
  let extract_ct_name_value header =
    let open Mrmime in
    let content_type =
      list_hd_map_or
        ~f:(function
          | Field.Field (_, Field.Content, content_type) ->
              Some (content_type : Content_type.t)
          | _ -> None)
        (Header.assoc Field_name.content_type header)
    in
    (match content_type with
    | None -> None
    | Some ct -> Content_type.Parameters.(find "name" (of_list ct.parameters)))
    |> Option.map (function `String str | `Token str -> str)

  let content_disposition_value_parser cd =
    let open Angstrom in
    (* TODO : Add a special header and a content-disposition module in
       mrmime instead of doing the parsing here*)
    let cd_value = string "attachment" <|> string "inline" in
    (* to define token *)
    let is_tspecials = function
      | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
      | ']' | '?' | '=' ->
          true
      | _ -> false
    in
    let is_ctl = function '\000' .. '\031' | '\127' -> true | _ -> false in
    let is_space = ( = ) ' ' in
    let is_ascii = function '\000' .. '\127' -> true | _ -> false in
    let is_token c =
      is_ascii c
      && (not (is_tspecials c))
      && (not (is_ctl c))
      && not (is_space c)
    in
    let is_wsp = function ' ' | '\t' -> true | _ -> false in
    let token = take_while1 is_token in
    (* From RFC 2045
          parameter := attribute "=" value
    *)
    let attribute = token >>| String.lowercase_ascii in
    let parameter =
      attribute >>= fun attribute ->
      skip_while is_wsp *> char '=' *> skip_while is_wsp *> token
      >>| fun value -> (attribute, value)
    in
    let parameters =
      skip_while is_wsp *> cd_value <* skip_while is_wsp >>= fun _cd_value ->
      many (skip_while is_wsp *> char ';' *> skip_while is_wsp *> parameter)
    in
    match parse_string ~consume:Consume.All parameters cd with
    | Ok v -> Some v
    | Error _ -> None

  (* Extract the parameter [filename] from [content-disposition] header if existed
     and returns its value. *)
  let extract_cd_filename_value header =
    let open Mrmime in
    let cd_field_name = Field_name.v "Content-Disposition" in
    let cd =
      list_hd_map_or
        ~f:(function
          | Field.Field (_, Field.Unstructured, cd_value) ->
              Some (cd_value : Unstructured.t)
          | _ -> None)
        (Header.assoc cd_field_name header)
    in
    (* Convert cd to string *)
    let cd =
      Option.map
        (fun cd ->
          let filter a = function
            | #Unstrctrd.elt as elt -> elt :: a
            | _ -> a
          in
          List.fold_left filter [] cd
          |> List.rev
          |> Unstrctrd.of_list
          |> Result.get_ok
          |> Unstrctrd.fold_fws
          |> Unstrctrd.to_utf_8_string)
        cd
    in
    let parameters =
      match cd with
      | None -> None
      | Some cd -> content_disposition_value_parser cd
    in
    match parameters with
    | None -> None
    | Some params -> List.assoc_opt "filename" params

  let partial_extract _ _ = []

  let forbidden_extension =
    [
      ".ade"; ".adp"; ".bat"; ".chm"; ".cmd"; ".com"; ".cpl"; ".exe"; ".hta";
      ".ins"; "isp"; ".jse"; ".lib"; ".mde"; ".msc"; ".msp"; ".mst"; ".pif";
      ".scr"; ".sct"; "shb"; ".sys"; ".vb"; ".vbe"; ".vbs"; ".vxd"; ".wsc";
      ".wsf"; ".wsh";
    ]

  let has_forbidden_extension filename_opt =
    Option.fold ~none:false
      ~some:(fun filename ->
        match Fpath.of_string filename with
        | Error _ -> false
        | Ok fpath -> List.mem (Fpath.get_ext fpath) forbidden_extension)
      filename_opt

  (* returns true if any header in the tree has a forbidden attachment *)
  let extract_from_header_tree (header, tree) =
    let has_attachment_with_forbidden_ext header =
      has_forbidden_extension (extract_cd_filename_value header)
      || has_forbidden_extension (extract_ct_name_value header)
    in
    let false_for_all f l = List.for_all (fun a -> not (f a)) l in
    let rec go = function
      | Mrmime.Mail.Leaf () -> false
      | Message (h, t) ->
          if has_attachment_with_forbidden_ext h then true else go t
      | Multipart parts ->
          false_for_all
            (fun (header, bodyopt) ->
              match bodyopt with
              | None -> false (* no body = no attachment, whatever the header *)
              | Some t -> has_attachment_with_forbidden_ext header && go t)
            parts
    in
    has_attachment_with_forbidden_ext header || go tree

  let add_partial (t : t) _ : t = t
  let write_db _oc _db = ()
  let read_db _ic = ()
  let train _ _ _ : db = ()
  let rank t _ = if t then [ 1. ] else [ 0. ]
end
