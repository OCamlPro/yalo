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

let file_kind_uids = ref 0
let file_uids = ref 0

let all_plugins = Hashtbl.create 13
let all_languages = Hashtbl.create 13
let all_namespaces = Hashtbl.create 13
let all_extensions = Hashtbl.create 13
let all_file_kinds = Hashtbl.create 13
let all_tags = Hashtbl.create 13
let all_nstags = Hashtbl.create 13
let all_warnings = ref []
let all_linters = ref []

let all_files = (Hashtbl.create 113 : (string, file) Hashtbl.t)
let all_projects = (Hashtbl.create 13 : (string, project) Hashtbl.t)

let active_warnings = ref StringMap.empty
let active_linters = ref []

let file_classifier = ref (fun _file_name -> None)

let messages = ref []

let get_messages () =
  let ms = List.rev !messages in
  messages := [];
  ms

let new_plugin ?(version="0.1.0") plugin_name =
  if Hashtbl.mem all_plugins plugin_name then begin
      Printf.eprintf "Configuration error: plugin %s is defined twice.\n%!" plugin_name ;
      Printf.eprintf "  Did you load twice the same plugin ?\n%!";
      exit 2
    end;
  let ns = { plugin_name ;
             plugin_version = version ;
             plugin_languages = StringMap.empty
           }
  in
  Hashtbl.add all_plugins plugin_name ns;
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
  lang

let new_file_kind lang ?(exts=[]) name checker =
  let kind_uid = !file_kind_uids in
  incr file_kind_uids ;
  (* TODO check uniqueness *)

  let file_kind = {
      kind_uid ;
      kind_language = lang ;
      kind_name = name ;
      kind_exts = exts ;
      kind_checker = checker ;
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
          linter_ns
          linter_name
          (*          ~level:linter_level *)
          ~warnings: linter_warnings
          ?(on_begin = fun _ -> ())
          ?(on_open =  fun ~file:_ -> ())
          ?(on_close = fun ~file:_ -> ())
          ?(on_end = fun _ -> ())
          linter_install =
  (* TODO: check uniqueness of linter_name *)
  if StringMap.mem linter_name linter_ns.ns_linters then begin
      Printf.eprintf "Configuration warning: plugin %S defines linter %S twice. Renaming it.\n%!"
        linter_ns.ns_name
        linter_name ;
      new_linter
        linter_lang
        linter_ns
        (linter_name ^ "X")
        (*        ~level:linter_level *)
        ~warnings:linter_warnings
        ~on_begin ~on_open ~on_close ~on_end
        linter_install
    end
  else
    let l = {
        linter_lang ;
        linter_namespace = linter_ns ;
        linter_name ;
        linter_warnings ;
        linter_install ;
        (*        linter_level ; *)
        linter_active = false ;
        linter_begin = on_begin ;
        linter_open = on_open ;
        linter_close = on_close ;
        linter_end = on_end ;
      } in
    linter_ns.ns_linters <- StringMap.add
                              linter_name l
                              linter_ns.ns_linters ;
    all_linters := l :: !all_linters

(*
let new_src_file_linter
      plugin
      name
      ~warnings
      ?on_begin ?on_end
      f =
  let linter_install l =
    active_src_file_linters := (l, f) :: !active_src_file_linters;
    active_linters := l :: !active_linters;
  in
  new_linter plugin name ~warnings ?on_begin ?on_end
          linter_install ~level:Linter_src_file
 *)

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

(* We suppose that all warnings are now correctly set. *)
let activate_linters () =
  List.iter (fun w ->
      if w.w_level_warning || w.w_level_error then
        active_warnings := StringMap.add w.w_idstr w !active_warnings
    ) !all_warnings ;
  List.iter (fun l ->
      if List.exists (fun w ->
             w.w_level_warning || w.w_level_error)
           l.linter_warnings then begin
          l.linter_active <- true;
          active_linters := l :: !active_linters;
          l.linter_install l
        end
    ) !all_linters

let warn msg_loc ~file ?msg w =
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
        msg_idstr ;
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
  List.iter (fun (l, f) ->
      try f ~file x with
      | exn ->
         Printf.eprintf "Configuration warning: linter %S raised exception %S while linting file %S\n%!"
           l.linter_name
           (Printexc.to_string exn)
           file.file_name
    ) linters

let iter_linters_open ~file linters =
  List.iter (fun (l,_) ->
      try
        l.linter_open ~file
      with exn ->
        Printf.eprintf "Configuration warning: linter %S raised exception %S while opening file %S\n%!"
          l.linter_name
          (Printexc.to_string exn)
          file.file_name
    ) linters

let iter_linters_close ~file linters =
  List.iter (fun (l,_) ->
      try
        l.linter_close ~file
      with exn ->
        Printf.eprintf "Configuration warning: linter %S raised exception %S while closing file %S\n%!"
          l.linter_name
          (Printexc.to_string exn)
          file.file_name
    ) linters

let rec filter_linters ~file linters = 
  match linters with
  | []  -> []
  | (l,f) :: linters ->
     if List.exists (fun w ->
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

let new_file ~file_kind ~file_crc file_name =
  let file_uid = !file_uids in
  incr file_uids ;
  {
    file_name ;
    file_uid ;
    file_crc ;
    file_kind ;
    file_projects = StringMap.empty ;
    file_messages = StringMap.empty ;
    file_done = false ;
    file_warnings_done = StringSet.empty ;
  }

let add_file ~file_kind ?p file_name =
  Printf.eprintf "add_file %s %s\n%!"
    file_name (match p with
    | None -> ""
    | Some p -> Printf.sprintf " (%s)" p.project_name);
  let file_crc = Digest.file file_name in
  let file =
    match Hashtbl.find all_files file_name with
    | file -> file
    | exception Not_found ->
       let file = new_file ~file_kind ~file_crc file_name in
       Hashtbl.add all_files file_name file;
       file
  in
  begin
    match p with
    | None -> ()
    | Some p ->
       file.file_projects <-
         StringMap.add p.project_name p file.file_projects
  end

let add_file ?file_kind ?p file_name =
  match file_kind with
  | Some file_kind ->
     add_file ~file_kind ?p file_name
  | None ->
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
            add_file ~file_kind ?p file_name
       else
         match !file_classifier file_name with
         | None -> ()
         | Some file_kind ->
            add_file ~file_kind ?p file_name
     in
     iter_ext extensions
