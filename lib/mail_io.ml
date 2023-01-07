let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

let parse fpath =
  let ic = open_in (Fpath.to_string fpath) in
  let ln = in_channel_length ic in
  let bs = Bytes.create ln in
  really_input ic bs 0 ln;
  close_in ic;
  match
    Angstrom.parse_string ~consume:All (Mrmime.Mail.mail None)
      (Bytes.unsafe_to_string bs)
  with
  | Ok v -> Ok v
  | Error _ -> Error (msgf "Invalid email: %a" Fpath.pp fpath)
