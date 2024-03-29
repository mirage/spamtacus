type label = [ `Spam | `Ham | `Unknown ]
type rank = float list
type ranks = (string * rank) list

module type FEATURE = sig
  val name : string

  type t

  val empty : t

  (* Extraction functions. *)
  val partial_extract : Mrmime.Header.t -> string -> string list
  val extract_from_header_tree : Mrmime.Header.t * unit Mrmime.Mail.t -> t
  val add_partial : t -> string list -> t

  (* Database related functions and type*)
  type db

  val empty_db : db

  (* [train] and [extract] are used for the same thing *)
  val train : db -> [ `Ham | `Spam ] -> t -> db
  val write_db : out_channel -> db -> unit
  val read_db : in_channel option -> db

  (* Ranking functions *)
  val rank : t -> db -> rank
end

type feature_vector = (module FEATURE) list

let create_fv () : feature_vector = []

let add_feature (f : (module FEATURE)) (fv : feature_vector) : feature_vector =
  f :: fv

module type FV = sig
  val vector : feature_vector
end

module type DT = sig
  val classify : ranks -> label
end

type training_set = { spam : Fpath.t; ham : Fpath.t }
type partial = { name : string; extracted : string list }

module type FILTER = sig
  (* Training functions *)
  val train_and_write_to_file : training_set -> output:Fpath.t -> unit

  (* Extraction functions *)
  val partial_extract : Mrmime.Header.t -> string -> partial list

  (* Ranking functions *)
  val instanciate :
    ?input_dir:Fpath.t ->
    (unit -> partial option Lwt.t) ->
    Mrmime.Header.t * unit Mrmime.Mail.t ->
    ranks Lwt.t

  val classify : ranks -> label

  (* Utilitary function *)
  val get_features_name : unit -> string list
end

module Filter (Features : FV) (DecisionTree : DT) : FILTER = struct
  let build_filename dirname (module Feature : FEATURE) =
    Fpath.add_seg dirname Feature.name |> Fpath.to_string

  let readdir dirpath =
    let dirpath' = Fpath.to_string dirpath in
    if Sys.is_directory dirpath' then
      Sys.readdir dirpath'
      |> Array.map (fun filename -> Fpath.add_seg dirpath filename)
      |> Array.to_list
    else failwith "todo"

  let to_stream (stream : 'a Lwt_stream.t) : unit -> 'a option Lwt.t =
   fun () -> Lwt_stream.get stream

  open Lwt.Infix

  let mail_to_header_tree header m : Mrmime.Header.t * unit Mrmime.Mail.t =
    let open Mrmime.Mail in
    let rec build = function
      | Leaf _ -> Leaf ()
      | Message (h, t) -> Message (h, build t)
      | Multipart parts ->
          let parts =
            List.map
              (fun (h, topt) ->
                match topt with
                | None -> (h, None)
                | Some t -> (h, Some (build t)))
              parts
          in
          Multipart parts
    in

    (header, build m)

  (* to improve *)
  let add_training_set (module F : FEATURE) training_set output =
    let rec build_feature h t = function
      | Mrmime.Mail.Leaf b -> (
          match F.partial_extract h b with
          | [] -> t
          | ext -> F.add_partial t ext)
      | Mrmime.Mail.Message (h, mail) -> build_feature h t mail
      | Mrmime.Mail.Multipart parts ->
          List.fold_left
            (fun t (h, body) ->
              match body with None -> t | Some m -> build_feature h t m)
            t parts
    in
    (* [add_to_db] add one-by-one the extracted parts of each email
       of the training set to the in-construction database *)
    let rec add_to_db db = function
      | [] -> db
      | (label, filename) :: xs -> (
          match Mail_io.parse filename with
          | Ok (h, mail) ->
              let header_tree = mail_to_header_tree h mail in
              let t = F.extract_from_header_tree header_tree in
              let t = build_feature h t mail in
              add_to_db (F.train db label t) xs
          | Error _ -> add_to_db db xs)
    in
    add_to_db F.empty_db training_set |> F.write_db output

  let train_and_write_to_file { spam; ham } ~output : unit =
    let spam_filename =
      readdir spam |> List.map (fun filename -> (`Spam, filename))
    in
    let ham_filename =
      readdir ham |> List.map (fun filename -> (`Ham, filename))
    in
    let training_set = spam_filename @ ham_filename in
    let rec go fv =
      match fv with
      | [] -> ()
      | (module F : FEATURE) :: fvs ->
          let oc = open_out (build_filename output (module F)) in
          add_training_set (module F) training_set oc;
          close_out oc;
          go fvs
    in
    go Features.vector

  let partial_extract (h : Mrmime.Header.t) (str : string) : partial list =
    List.fold_left
      (fun acc (module F : FEATURE) ->
        match F.partial_extract h str with
        | [] -> List.rev acc
        | extracted -> { name = F.name; extracted } :: acc)
      [] Features.vector

  let instanciate ?input_dir (input_stream : unit -> partial option Lwt.t)
      (header_tree : Mrmime.Header.t * unit Mrmime.Mail.t) : ranks Lwt.t =
    let rec loop (fv : feature_vector) (ranks, stream) =
      match fv with
      | [] -> Lwt.return (List.rev ranks)
      | (module F : FEATURE) :: fvs ->
          let ic =
            match input_dir with
            | Some input_dir ->
                Some (open_in (build_filename input_dir (module F)))
            | None -> None
          in
          let db = F.read_db ic in
          let copy, pusher = Lwt_stream.create () in
          let t = F.extract_from_header_tree header_tree in
          let rec go t =
            stream () >>= function
            | None ->
                (try pusher None with Lwt_stream.Closed -> ());
                Lwt.return ((F.name, F.rank t db) :: ranks, copy)
            | Some ({ name; extracted } as partial) ->
                if name = F.name then go (F.add_partial t extracted)
                else (
                  pusher (Some partial);
                  go t)
          in
          go t >>= fun (ranks, copy) -> loop fvs (ranks, to_stream copy)
    in
    loop Features.vector ([], input_stream)

  let classify : ranks -> label = DecisionTree.classify

  let get_features_name () =
    List.map (fun (module Feature : FEATURE) -> Feature.name) Features.vector
end
