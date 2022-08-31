let read fpath =
  let filename = Fpath.to_string fpath in
  let ic = open_in filename in
  (* drop first line *)
  let first_line = input_line ic in
  let acc =
    if String.sub first_line 0 5 = "From " then [] else [ first_line ]
  in
  let rec go acc =
    match input_line ic with
    | line -> go ((line ^ "\r\n") :: acc)
    | exception End_of_file -> List.rev acc
  in
  let lines = go acc in
  close_in ic;
  String.concat "" lines

exception ParsingError of string

let parse fpath =
  let mail_str = read fpath in
  match Angstrom.parse_string ~consume:All (Mrmime.Mail.mail None) mail_str with
  | Ok s -> s
  | Error s -> raise (ParsingError s)
