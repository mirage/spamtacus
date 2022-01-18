module AntiVirus = Spamtacus_bayesian.BasicAntiVirus
open Mrmime

let rec list_hd_map_or ~f = function
  | [] -> None
  | x :: r -> ( match f x with Some x -> Some x | None -> list_hd_map_or ~f r)

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

let hdr1 =
  let mime_version =
    Rresult.R.failwith_error_msg (Unstructured.of_string " 1.0\r\n")
  in
  let content_type =
    Content_type.(
      make `Text
        (Subtype.iana_exn `Text "plain")
        Content_type.Parameters.(of_list [ (k "name", v "virus.exe") ]))
  in
  Header.empty
  |> Header.add Field_name.mime_version
       (Field.Unstructured, (mime_version :> Unstructured.t))
  |> Header.add Field_name.content_type (Field.Content, content_type)

let dummy_mail = Mail.Leaf ()
let test1 () = AntiVirus.extract_from_header_tree (hdr1, dummy_mail)

let hdr2 =
  let mime_version =
    Rresult.R.failwith_error_msg (Unstructured.of_string " 1.0\r\n")
  in
  let content_type =
    Content_type.(
      make `Text
        (Subtype.iana_exn `Text "plain")
        Content_type.Parameters.(of_list [ (k "name", v "virus.exe") ]))
  in
  let content_disposition_value =
    Rresult.R.failwith_error_msg
      (Unstructured.of_string " inline;filename=spam.exe\r\n")
  in
  Header.empty
  |> Header.add Field_name.mime_version
       (Field.Unstructured, (mime_version :> Unstructured.t))
  |> Header.add
       (Field_name.v "Content-Disposition")
       (Field.Unstructured, (content_disposition_value :> Unstructured.t))
  |> Header.add Field_name.content_type (Field.Content, content_type)

let test2 () = AntiVirus.extract_from_header_tree (hdr2, dummy_mail)

let mail () =
  Format.printf "Test 1 : %f\n"
    (AntiVirus.rank (test1 ()) AntiVirus.empty_db |> List.hd);
  Format.printf "Test 2 : %f@."
    (AntiVirus.rank (test2 ()) AntiVirus.empty_db |> List.hd)
;;

mail ()
