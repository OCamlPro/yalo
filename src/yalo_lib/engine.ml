(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open EzCompat (* for IntMap *)
open Types
open Config.OP
open Yalo_misc.Utils.OP

let file_kind_uids = ref 0
let file_uids = ref 0

let verbosity = ref 0
let verbose n = !verbosity >= n

let profiles_fileattrs = ref ([] : (string * Types.file_attr list) list)
let profiles_load_dirs = ref ([] : string list)
let profiles_plugins = ref ([] : string list)
let profiles_profiles = ref ([] : string list)
let profiles_warnings = ref ([] : string list)
let profiles_errors = ref ([] : string list)

let all_plugins = Hashtbl.create 13
let all_languages = Hashtbl.create 13
let all_namespaces = Hashtbl.create 13
let all_extensions = Hashtbl.create 13
let all_file_kinds = Hashtbl.create 13
let all_tags = Hashtbl.create 13
let all_nstags = Hashtbl.create 13
let all_warnings = ref []
let all_linters = ref []
let all_plugins_args = ref []

let all_files = (Hashtbl.create 113 : (string, file) Hashtbl.t)
let all_projects = (Hashtbl.create 13 : (string, project) Hashtbl.t)

let active_warnings = ref StringMap.empty
let active_linters = ref []

let file_classifier = ref (fun ~file_doc:_ -> None)
let folder_updater = ref (fun ~folder:(_:folder) -> ())

let messages = ref []

let get_messages () =
  let ms = List.rev !messages in
  messages := [];
  ms

let new_plugin ?(version="0.1.0") ?(args=[]) plugin_name =
  if Hashtbl.mem all_plugins plugin_name then begin
      Printf.eprintf "Configuration error: plugin %s is defined twice.\n%!" plugin_name ;
      Printf.eprintf "  Did you load twice the same plugin ?\n%!";
      exit 2
    end;
  let ns = { plugin_name ;
             plugin_version = version ;
             plugin_languages = StringMap.empty ;
             plugin_args = args;
           }
  in
  Hashtbl.add all_plugins plugin_name ns;
  if verbose 1 then
    Printf.eprintf "  Plugin %S installed\n%!" plugin_name ;
  ns

let new_language plugin lang_name =
  if Hashtbl.mem all_languages lang_name then begin
      Printf.eprintf "Configuration error: language %s is defined twice.\n%!" lang_name ;
      Printf.eprintf "  Did you load twice the same plugin ?\n%!";
      exit 2
    end;
  let lang = { lang_name ;
               lang_plugin = plugin ;
               lang_kinds = StringMap.empty ;
             }
  in
  Hashtbl.add all_languages lang_name lang;
  plugin.plugin_languages <- StringMap.add lang_name lang
                               plugin.plugin_languages ;
  if verbose 1 then
    Printf.eprintf "  Language %S installed\n%!" lang_name ;
  lang

let new_file_kind ~lang ?(exts=[]) ~name
      ?(validate= fun ~file_doc:_ -> true)
      ~lint () =
  let kind_uid = !file_kind_uids in
  incr file_kind_uids ;
  (* TODO check uniqueness *)

  let file_kind = {
      kind_uid ;
      kind_language = lang ;
      kind_name = name ;
      kind_exts = exts ;
      kind_validate = validate ;
      kind_lint = lint ;
    } in
  lang.lang_kinds <- StringMap.add name file_kind lang.lang_kinds;
  List.iter (fun ext ->
      begin
        match Hashtbl.find all_extensions ext with
        | exception Not_found -> ()
        | f2 ->
           Printf.eprintf "Configuration warning: extension %S used by %S now used by %S\n%!"
             ext f2.kind_name file_kind.kind_name
      end;
      Hashtbl.add all_extensions ext file_kind
    ) exts ;
  Hashtbl.add all_file_kinds kind_uid file_kind ;
  file_kind

let new_namespace plugin ns_name =
  for i = 0 to String.length ns_name - 1 do
    match ns_name.[i] with
    | 'A'..'Z' -> ()
    | '0'..'9' | '_' when i>0 -> ()
    | c ->
       Printf.eprintf "Configuration error: namespace %S contains illegal character '%c'\n%!" ns_name c;
       exit 2
  done;
  if Hashtbl.mem all_namespaces ns_name then begin
      Printf.eprintf "Configuration error: namespace %s is defined twice.\n%!" ns_name ;
      Printf.eprintf "  Did you load twice the same plugin ?\n%!";
      exit 2
    end;
  let ns = { ns_name ;
             ns_plugin = plugin ;
             ns_warnings = IntMap.empty ;
             ns_linters = StringMap.empty;
           } in
  Hashtbl.add all_namespaces ns_name ns;
  if verbose 1 then
    Printf.eprintf "  Namespace %S installed\n%!" ns_name ;
  ns

let new_tag tag_name =
  for i = 0 to String.length tag_name - 1 do
    match tag_name.[i] with
    | 'a'..'z' -> ()
    | '0'..'9' | '_' when i>0 -> ()
    | c ->
       Printf.eprintf "Configuration error: tag_name %S contains illegal character '%c'\n%!" tag_name c;
       exit 2
  done;
  try
    Hashtbl.find all_tags tag_name
  with Not_found ->
    let tag = { tag_name ; tag_warnings = [] } in
    Hashtbl.add all_tags tag_name tag;
    tag

let add_tag w tag =
  w.w_tags <- tag :: w.w_tags ;
  tag.tag_warnings <- w :: tag.tag_warnings ;
  let nstag = Printf.sprintf "%s:%s"
                w.w_namespace.ns_name tag.tag_name in
  let r =
    try
      Hashtbl.find all_nstags nstag
    with Not_found ->
          let r = ref [] in
          Hashtbl.add all_nstags nstag r;
          r
  in
  r := w :: !r

let new_warning
      w_namespace
      ?(tags=[])
      ?(desc="")
      ~name:w_name
      ~msg:w_msg
      w_num
  =
  let w_idstr = Printf.sprintf "%s+%d" w_namespace.ns_name  w_num
  in
  let w = {
      w_namespace ;
      w_num ;
      w_idstr ;
      w_name ;
      w_tags = [] ;
      w_linters = StringMap.empty ;
      w_msg ;
      w_level_warning = false ;
      w_level_error = false ;
      w_desc = desc ;
    } in
  begin
    match IntMap.find w_num w_namespace.ns_warnings with
    | exception Not_found -> (* good *) ()
    | w0 ->
       Printf.eprintf
         "Configuration error: in plugin %s, warning %d is defined twice:\n%!"
         w_namespace.ns_name w_num;
       Printf.eprintf "  * for warning %S\n%!" w0.w_name;
       Printf.eprintf "  * for warning %S\n%!" w.w_name;
       exit 2
  end;
  w_namespace.ns_warnings <- IntMap.add w_num w w_namespace.ns_warnings;
  all_warnings := w :: !all_warnings ;
  List.iter (add_tag w) tags;
  w

let rec new_linter
          linter_lang
          ns
          linter_name
          ~warnings
          ?(on_begin = fun _ -> ())
          ?(on_open =  fun ~file:_ ~linter:_ -> ())
          ?(on_close = fun ~file:_ ~linter:_ -> ())
          ?(on_end = fun _ -> ())
          linter_install =
  (* TODO: check uniqueness of linter_name *)
  if StringMap.mem linter_name ns.ns_linters then begin
      Printf.eprintf
        "Developer warning: plugin %S defines linter %S twice. Renaming it.\n%!"
        ns.ns_name
        linter_name ;
      new_linter
        linter_lang
        ns
        (linter_name ^ "X")
        ~warnings
        ~on_begin ~on_open ~on_close ~on_end
        linter_install
    end
  else
    let linter_warnings =
      StringMap.of_list (List.map (fun w -> (w.w_idstr, w)) warnings)
    in
    let l = {
        linter_lang ;
        linter_namespace = ns ;
        linter_idstr =
          Printf.sprintf "%s:%s" ns.ns_name linter_name ;
        linter_name ;
        linter_warnings ;
        linter_install ;
        linter_active = false ;
        linter_begin = on_begin ;
        linter_open = on_open ;
        linter_close = on_close ;
        linter_end = on_end ;
      } in
    ns.ns_linters <- StringMap.add
                       linter_name l
                       ns.ns_linters ;
    all_linters := l :: !all_linters ;
    List.iter (fun w ->
        w.w_linters <-
          StringMap.add l.linter_idstr l w.w_linters)
      warnings

let new_gen_linter lang active_linters_ref =
  fun
    ns
    name
    ~warnings
    ?on_begin ?on_open ?on_close ?on_end
    f ->
  let linter_install l =
    active_linters_ref := (l, f) :: !active_linters_ref ;
  in
  new_linter lang ns name ~warnings ?on_begin ?on_open ?on_close ?on_end
    linter_install

exception LocalExit
let local_exit = LocalExit
let stringMap_exists p map =
  try
    StringMap.iter (fun _ x -> if p x then raise local_exit) map;
    false
  with LocalExit -> true

(* We suppose that all warnings are now correctly set. *)
let activate_linters () =
  List.iter (fun w ->
      if StringMap.is_empty w.w_linters then
        Printf.eprintf
          "Developer warning: warning %s has no associated linter\n%!"
             w.w_idstr;
      if w.w_level_warning || w.w_level_error then
        active_warnings := StringMap.add w.w_idstr w !active_warnings
    ) !all_warnings ;
  List.iter (fun l ->
      if stringMap_exists (fun w ->
             w.w_level_warning || w.w_level_error)
           l.linter_warnings then begin
          l.linter_active <- true;
          active_linters := l :: !active_linters;
          l.linter_install l
        end
    ) !all_linters

let warn msg_loc ~file ~linter ?msg ?(autofix=[]) w =

  if not ( StringMap.mem w.w_idstr linter.linter_warnings ) then begin
      Printf.eprintf
        "Developer warning: warning %S is not declared for linter %S\n%!"
        w.w_idstr linter.linter_name;
    end;

  if w.w_level_error || w.w_level_warning then
    let msg_string =
      match msg with
      | None -> w.w_msg
      | Some msg -> msg
    in
    let msg_idstr =
      Printf.sprintf "%06d%06d%s"
        msg_loc.loc_start.pos_lnum
        msg_loc.loc_start.pos_cnum
        (Marshal.to_string (msg_loc,w.w_idstr,msg) [])
    in
    let m = {
        msg_loc ;
        msg_string ;
        msg_warning = w;
        msg_file = file ;
        msg_linter = linter ;
        msg_idstr ;
        msg_autofix = autofix ;
      } in
    file.file_messages <- StringMap.add msg_idstr m file.file_messages ;
    messages := m :: !messages

let mkloc ~bol ?(start_cnum=bol) ?(end_cnum=start_cnum)
      ~lnum ~file () =
  let loc_start =
    Lexing.{
        pos_fname = file.file_name ;
        pos_lnum =  lnum ;
        pos_bol = bol ;
        pos_cnum = start_cnum ;
    }
  in
  let loc_end =
    Lexing.{ loc_start with
             pos_cnum = end_cnum }
  in
  Location.{
      loc_start ;
      loc_end ;
      loc_ghost = true;
  }

let iter_linters ~file linters x =
  List.iter (fun (linter, f) ->
      try f ~file ~linter x with
      | exn ->
         Printf.eprintf "Configuration warning: linter %S raised exception %S while linting file %S\n%!"
           linter.linter_name
           (Printexc.to_string exn)
           file.file_name
    ) linters

let iter_linters_open ~file linters =
  List.iter (fun (linter,_) ->
      try
        linter.linter_open ~file ~linter
      with exn ->
        Printf.eprintf "Configuration warning: linter %S raised exception %S while opening file %S\n%!"
          linter.linter_name
          (Printexc.to_string exn)
          file.file_name
    ) linters

let iter_linters_close ~file linters =
  List.iter (fun (linter,_) ->
      try
        linter.linter_close ~file ~linter
      with exn ->
        Printf.eprintf "Configuration warning: linter %S raised exception %S while closing file %S\n%!"
          linter.linter_name
          (Printexc.to_string exn)
          file.file_name
    ) linters

let rec filter_linters ~file linters =
  match linters with
  | []  -> []
  | (l,f) :: linters ->
     if stringMap_exists (fun w ->
            (w.w_level_warning || w.w_level_error) &&
              not (StringSet.mem w.w_idstr file.file_warnings_done)
          ) l.linter_warnings then
       (l,f) :: filter_linters ~file linters
     else
       filter_linters ~file linters

let lint_with_active_linters active_linters_ref =
  fun ~file x ->
  match filter_linters ~file !active_linters_ref with
  | [] -> ()
  | linters ->
     iter_linters_open ~file linters ;
     iter_linters ~file linters x ;
     iter_linters_close ~file linters ;
     ()

let new_file ~file_doc ~file_kind ~file_crc file_name =
  let file_uid = !file_uids in
  incr file_uids ;
  {
    file_name ;
    file_uid ;
    file_crc ;
    file_kind ;
    file_doc ;
    file_projects = file_doc.doc_parent.folder_projects ;
    file_messages = StringMap.empty ;
    file_done = false ;
    file_warnings_done = StringSet.empty ;
  }

let string_of_projects map =
  StringMap.to_list map |>
    List.map fst |>
    String.concat ":"

let add_file ~file_doc ~file_kind =
  if file_kind.kind_validate ~file_doc then
    let file_name = file_doc.doc_name in
    if verbose 2 then
      Printf.eprintf "add_file %s %s\n%!"
        file_name (string_of_projects file_doc.doc_parent.folder_projects);

    let file_crc = Digest.file file_name in
    match Hashtbl.find all_files file_name with
    | _file -> assert false
    | exception Not_found ->
       let file = new_file ~file_doc ~file_kind ~file_crc file_name in
       Hashtbl.add all_files file_name file;
       file_doc.doc_file <- Some file ;
       ()

let doc_kind ~file_doc =
  let file_name = file_doc.doc_name in
  let basename = Filename.basename file_name in
  let _basename, extensions = EzString.cut_at basename '.' in
  let extensions = String.lowercase extensions in

  let rec iter_ext ext =
    if ext <> "" then
      match Hashtbl.find all_extensions ext with
      | exception Not_found ->
         let _, ext = EzString.cut_at ext '.' in
         iter_ext ext
      | file_kind ->
         Some file_kind
    else
      match !file_classifier ~file_doc with
      | None ->
         None
      | Some file_kind ->
         Some file_kind
  in
  iter_ext extensions

let add_file ~file_doc =
  match doc_kind ~file_doc with
  | None ->
     let file_name = file_doc.doc_name in
     if verbose 2 then
       Printf.eprintf "Skipping file %S\n%!" file_name
  | Some file_kind ->
     add_file ~file_doc ~file_kind

let add_plugin_args plugin specs =
  plugin.plugin_args <- plugin.plugin_args @ specs ;
  all_plugins_args := !all_plugins_args @ specs ;
  ()

let new_project name =
  match Hashtbl.find all_projects name with
  | p -> p
  | exception Not_found ->
     let p = {
         project_name = name ;
         project_files = [] ;
       } in
     Hashtbl.add all_projects name p ;
     p

let project_map_add ?(map=StringMap.empty) p =
  StringMap.add p.project_name p map

let new_fs ~fs_root ~fs_subpath =

  let project_all = new_project "_" in
  let project_default = new_project "default" in
  let rec fs_folder = {
      folder_fs = fs ;
      folder_parent = fs_folder ;
      folder_basename = "";
      folder_name = "";
      folder_tags = StringSet.empty ;
      folder_docs = StringMap.empty ;
      folder_folders = StringMap.empty ;
      folder_projects = project_map_add project_default ;
      folder_scan = Scan_disabled ;
    }
  and fs =
  {
    fs_root ;
    fs_project = project_all ;
    fs_folder ;
    fs_subpath ;
  }
  in
  fs

let get_folder folder_parent basename =
  match StringMap.find basename folder_parent.folder_folders with
  | folder -> folder
  | exception Not_found ->
     let folder = {
         folder_fs = folder_parent.folder_fs ;
         folder_parent ;
         folder_basename = basename ;
         folder_name = folder_parent.folder_name // basename ;
         folder_tags = StringSet.empty ;
         folder_docs = StringMap.empty ;
         folder_folders = StringMap.empty ;
         folder_projects = folder_parent.folder_projects ;
         folder_scan = Scan_disabled ;
       } in
     folder_parent.folder_folders <-
       StringMap.add basename folder
         folder_parent.folder_folders;
     folder

let get_document doc_parent basename =
  match StringMap.find basename doc_parent.folder_docs with
  | doc -> doc
  | exception Not_found ->
     let file_doc = {
         doc_parent ;
         doc_basename = basename ;
         doc_name = doc_parent.folder_name // basename;
         doc_tags = StringSet.empty ;
         doc_file = None ;
       } in
     doc_parent.folder_docs <-
       StringMap.add basename file_doc
         doc_parent.folder_docs;
     file_doc

let add_file_classifier f =
  let old_f = !file_classifier in
  file_classifier := (fun ~file_doc ->
    match f ~file_doc with
    | Some file_kind -> Some file_kind
    | None -> old_f ~file_doc)

let profile_append ( profile_var, profile_option ) =
  profile_var := !profile_var @ !!profile_option ;
  profile_option =:= []

let eprint_config () =
  Printf.eprintf "Engine.config:\n%!";
  Printf.eprintf "  * Active warnings: %d\n%!"
    (StringMap.cardinal !active_warnings);
  Printf.eprintf "  * Active linters: %d\n%!" (List.length !active_linters);
  ()


let add_folder_updater f =
  let old_f = !folder_updater in
  folder_updater := (fun ~folder ->
    old_f ~folder ;
    f ~folder)

let update_warnings ~file ~loc spec =
  ignore (file, loc, spec);
  ()
