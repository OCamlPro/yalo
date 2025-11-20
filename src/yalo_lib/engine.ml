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
open Yalo_misc.Infix

let string_of_loc loc =
  Location.print_loc Format.str_formatter loc;
  Format.flush_str_formatter ()

let eprintf ?loc fmt =
  begin
    match loc with
    | Some loc -> Printf.eprintf "%s\n" (string_of_loc loc)
    | None -> ()
  end;
  Printf.eprintf fmt

let verbose n = !GState.verbosity >= n

let new_plugin ?(version="0.1.0") ?(args=[]) plugin_name =
  if Hashtbl.mem GState.all_plugins plugin_name then begin
    Printf.eprintf
      "Configuration error: plugin %s is defined twice.\n%!" plugin_name ;
    Printf.eprintf "  Did you load twice the same plugin ?\n%!";
    exit 2
  end;
  let ns = { plugin_name ;
             plugin_version = version ;
             plugin_languages = StringMap.empty ;
             plugin_args = args;
           }
  in
  Hashtbl.add GState.all_plugins plugin_name ns;
  if verbose 1 then
    Printf.eprintf "  Plugin %S installed\n%!" plugin_name ;
  ns

let new_language plugin lang_name =
  if Hashtbl.mem GState.all_languages lang_name then begin
    Printf.eprintf
      "Configuration error: language %s is defined twice.\n%!" lang_name ;
    Printf.eprintf "  Did you load twice the same plugin ?\n%!";
    exit 2
  end;
  let lang = { lang_name ;
               lang_plugin = plugin ;
               lang_kinds = StringMap.empty ;
             }
  in
  Hashtbl.add GState.all_languages lang_name lang;
  plugin.plugin_languages <- StringMap.add lang_name lang
      plugin.plugin_languages ;
  if verbose 1 then
    Printf.eprintf "  Language %S installed\n%!" lang_name ;
  lang

let new_file_kind ~lang ?(exts=[]) ~name
    ?(validate= fun ~file_doc:_ -> true)
    ~lint () =
  let kind_uid = GState.new_file_kind_uid () in
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
        match Hashtbl.find GState.all_extensions ext with
        | exception Not_found -> ()
        | f2 ->
            Printf.eprintf
              "Configuration warning: extension %S used by %S and %S\n%!"
              ext f2.kind_name file_kind.kind_name
      end;
      Hashtbl.add GState.all_extensions ext file_kind
    ) exts ;
  Hashtbl.add GState.all_file_kinds kind_uid file_kind ;
  file_kind

let new_namespace plugin ns_name =
  for i = 0 to String.length ns_name - 1 do
    match ns_name.[i] with
    | 'A'..'Z' -> ()
    | '0'..'9' | '_' when i>0 -> ()
    | c ->
        Printf.eprintf
          "Configuration error: namespace %S contains illegal \
           character '%c'\n%!"
          ns_name c;
        exit 2
  done;
  if Hashtbl.mem GState.all_namespaces ns_name then begin
    Printf.eprintf
      "Configuration error: namespace %s is defined twice.\n%!" ns_name ;
    Printf.eprintf "  Did you load twice the same plugin ?\n%!";
    exit 2
  end;
  let ns = { ns_name ;
             ns_plugin = plugin ;
             ns_warnings_by_num = IntMap.empty ;
             ns_warnings_by_name = StringMap.empty ;
             ns_linters = StringMap.empty;
           } in
  Hashtbl.add GState.all_namespaces ns_name ns;
  if verbose 1 then
    Printf.eprintf "  Namespace %S installed\n%!" ns_name ;
  ns

let new_tag tag_name =
  for i = 0 to String.length tag_name - 1 do
    match tag_name.[i] with
    | 'a'..'z' -> ()
    | '0'..'9' | '_' when i>0 -> ()
    | c ->
        Printf.eprintf
          "Configuration error: tag_name %S contains illegal character '%c'\n%!"
          tag_name c;
        exit 2
  done;
  try
    Hashtbl.find GState.all_tags tag_name
  with Not_found ->
    let tag = { tag_name ; tag_warnings = [] } in
    Hashtbl.add GState.all_tags tag_name tag;
    tag

let add_tag w tag =
  w.w_tags <- tag :: w.w_tags ;
  tag.tag_warnings <- w :: tag.tag_warnings ;
  let nstag = Printf.sprintf "%s:%s"
      w.w_namespace.ns_name tag.tag_name in
  let r =
    try
      Hashtbl.find GState.all_nstags nstag
    with Not_found ->
      let r = ref [] in
      Hashtbl.add GState.all_nstags nstag r;
      r
  in
  r := w :: !r

let new_warning
    w_namespace
    ?(tags=[])
    ?(desc="")
    ?(set_by_default=true)
    ~name:w_name
    ~msg:w_msg
    w_num
  =
  for i = 0 to String.length w_name - 1 do
    match w_name.[i] with
    | 'a'..'z' -> ()
    | '0'..'9' | '_' | ':'
      when i>0 -> ()
    | c ->
        Printf.eprintf
          "Configuration error: warning %S contains illegal character '%c'\n%!"
          w_name c;
        exit 2
  done;

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
    w_set_by_default = set_by_default ;
    w_state = Warning_disabled ;
    w_level_error = false ;
    w_desc = desc ;
  } in
  begin
    match IntMap.find w_num w_namespace.ns_warnings_by_num with
    | exception Not_found -> (* good *) ()
    | w0 ->
        Printf.eprintf
          "Configuration error: in plugin %s, warning %d is defined twice:\n%!"
          w_namespace.ns_name w_num;
        Printf.eprintf "  * for warning %S\n%!" w0.w_name;
        Printf.eprintf "  * for warning %S\n%!" w.w_name;
        exit 2
  end;
  begin
    match StringMap.find w_name w_namespace.ns_warnings_by_name with
    | exception Not_found -> (* good *) ()
    | w0 ->
        Printf.eprintf
          "Configuration error: in plugin %S, warning %S is defined twice:\n%!"
          w_namespace.ns_name w_name;
        Printf.eprintf "  * for warning %s\n%!" w0.w_idstr;
        Printf.eprintf "  * for warning %s\n%!" w.w_idstr;
        exit 2
  end;
  w_namespace.ns_warnings_by_num <-
    IntMap.add w_num w w_namespace.ns_warnings_by_num;
  w_namespace.ns_warnings_by_name <-
    StringMap.add w_name w w_namespace.ns_warnings_by_name;
  GState.all_warnings := w :: !GState.all_warnings ;
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
    GState.all_linters := l :: !GState.all_linters ;
    List.iter (fun w ->
        w.w_linters <-
          StringMap.add l.linter_idstr l w.w_linters)
      warnings

let new_gen_linter lang active_linters_ref
    ns
    name
    ~warnings
    ?on_begin ?on_open ?on_close ?on_end
    f =
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
      match w.w_state with
      | Warning_disabled -> ()
      | Warning_sleeping
      | Warning_enabled ->
          GState.active_warnings := StringMap.add w.w_idstr w
              !GState.active_warnings
    ) !GState.all_warnings ;
  List.iter (fun l ->
      if stringMap_exists (fun w ->
          match w.w_state with
          | Warning_disabled -> false
          | Warning_sleeping
          | Warning_enabled -> true)
          l.linter_warnings then begin
        l.linter_active <- true;
        GState.active_linters := l :: !GState.active_linters;
        l.linter_install l
      end
    ) !GState.all_linters

let get_target name =
  let name = Yalo_misc.Utils.normalize_filename name in
  match Hashtbl.find GState.all_targets name with
  | target -> target
  | exception Not_found ->
      let target = {
        target_name = name ;
        target_uid = GState.new_target_uid () ;

        target_checks = [] ;
        target_zones = [] ;
        target_messages = [] ;
      } in
      Hashtbl.add GState.all_targets name target ;
      target

let warnings_check ~file:_ ~loc spec after =
  let target_name = loc.loc_start.pos_fname in
  let target = get_target target_name in
  target.target_checks <- (spec, loc, after) :: target.target_checks

let warnings_zone ~file ~loc ?(mode=Zone_begin) action =
  let pos = loc.loc_start in
  let target_name = pos.pos_fname in
  let target = get_target target_name in
  let zone = {
    zone_creator = file ;
    zone_loc = loc ;
    zone_target = target ;
    zone_spec = action ;
    zone_rev_zone = None ;
    zone_rev_changes = [];
  }
  in
  target.target_zones <- zone :: target.target_zones ;
  match mode with
  | Zone_begin -> ()
  | Zone_all ->
      let pos = loc.loc_end in
      if target_name = pos.pos_fname then
        let zone = {
          zone_creator = file ;
          zone_loc = { loc with loc_start = pos } ;
          zone_target = target ;
          zone_rev_zone = Some zone ;
          zone_spec = action ;
          zone_rev_changes = [];
        }
        in
        target.target_zones <- zone :: target.target_zones ;
        ()

let warn ~loc ~file ~linter ?msg ?(autofix=[]) w =

  if not ( StringMap.mem w.w_idstr linter.linter_warnings ) then begin
    Printf.eprintf
      "Developer warning: warning %S is not declared for linter %S\n%!"
      w.w_idstr linter.linter_name;
  end;

  match w.w_state with
  | Warning_disabled -> ()
  | Warning_sleeping
  | Warning_enabled ->
      if verbose 2 then
        Printf.eprintf "Warning %S in %s:%d set by linter %S scanning %S\n%!"
          w.w_idstr loc.loc_start.pos_fname loc.loc_start.pos_lnum
          linter.linter_name file.file_name;

      (*     if not loc.loc_ghost then *)
      let msg_string =
        match msg with
        | None -> w.w_msg
        | Some msg -> msg
      in
      let msg_idstr =
        Printf.sprintf "%06d%06d%s"
          loc.loc_start.pos_lnum
          loc.loc_start.pos_cnum
          (Marshal.to_string (loc,w.w_idstr,msg) [])
      in
      let target_name = loc.loc_start.pos_fname in
      let target = get_target target_name in
      let m = {
        msg_loc = loc ;
        msg_string ;
        msg_warning = w;
        msg_file = file ;
        msg_linter = linter ;
        msg_idstr ;
        msg_autofix = autofix ;
        msg_target = target ;
      } in
      file.file_messages <- StringMap.add msg_idstr m file.file_messages ;
      GState.messages := m :: !GState.messages ;

      target.target_messages <- m :: target.target_messages ;
      GState.message_targets := IntMap.add target.target_uid target
          !GState.message_targets;
      ()

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
    loc_ghost = false;
  }

let iter_linters ~file linters x =
  List.iter (fun (linter, f) ->
      try f ~file ~linter x with
      | exn ->
          Printf.eprintf
            "Internal warning: linter %S raised exception %S
            while linting file %S\n%!"
            linter.linter_name
            (Printexc.to_string exn)
            file.file_name
    ) linters

let iter_linters_open ~file linters =
  List.iter (fun (linter,_) ->
      try
        linter.linter_open ~file ~linter
      with exn ->
        Printf.eprintf "Internal Warning: linter %S raised exception %S \
                        while opening file %S\n%!"
          linter.linter_name
          (Printexc.to_string exn)
          file.file_name
    ) linters

let iter_linters_close ~file linters =
  List.iter (fun (linter,_) ->
      try
        linter.linter_close ~file ~linter
      with exn ->
        Printf.eprintf
          "Configuration warning: linter %S raised exception %S \
           while closing file %S\n%!"
          linter.linter_name
          (Printexc.to_string exn)
          file.file_name
    ) linters

let rec filter_linters ~file linters =
  match linters with
  | []  -> []
  | (l,f) :: linters ->
      if stringMap_exists (fun w ->
          (match w.w_state with
           | Warning_disabled -> false
           | Warning_enabled
           | Warning_sleeping -> true) &&
          not (StringSet.mem w.w_idstr file.file_warnings_done)
        ) l.linter_warnings then
        (l,f) :: filter_linters ~file linters
      else
        filter_linters ~file linters

let lint_with_active_linters active_linters_ref ~file x =
  match filter_linters ~file !active_linters_ref with
  | [] -> ()
  | linters ->
      iter_linters_open ~file linters ;
      iter_linters ~file linters x ;
      iter_linters_close ~file linters ;
      ()

let new_file ~file_doc ~file_kind ~file_crc file_name =
  {
    file_name ;
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
  let file_name = file_doc.doc_name in
  if verbose 2 then
    Printf.eprintf "add_file %s %s\n%!"
      file_name (string_of_projects file_doc.doc_parent.folder_projects);

  let file_crc = Digest.file file_name in
  match Hashtbl.find GState.all_files file_name with
  | _file -> assert false
  | exception Not_found ->
      let file = new_file ~file_doc ~file_kind ~file_crc file_name in
      Hashtbl.add GState.all_files file_name file;
      file_doc.doc_file <- Some file ;
      ()

let doc_kind ~file_doc =
  let file_name = file_doc.doc_name in
  let basename = Filename.basename file_name in
  let _basename, extensions = EzString.cut_at basename '.' in
  let extensions = String.lowercase extensions in

  let rec iter_ext ext =
    if ext <> "" then
      match Hashtbl.find GState.all_extensions ext with
      | exception Not_found ->
          let _, ext = EzString.cut_at ext '.' in
          iter_ext ext
      | file_kind ->
          Some file_kind
    else
      match !GState.file_classifier ~file_doc with
      | None ->
          None
      | Some file_kind ->
          Some file_kind
  in
  iter_ext extensions

let add_plugin_args plugin specs =
  plugin.plugin_args <- plugin.plugin_args @ specs ;
  GState.all_plugins_args := !GState.all_plugins_args @ specs ;
  ()

let new_project name =
  match Hashtbl.find GState.all_projects name with
  | p -> p
  | exception Not_found ->
      let p = {
        project_name = name ;
        project_files = [] ;
      } in
      Hashtbl.add GState.all_projects name p ;
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
    folder_other_names = [] ;
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
        folder_other_names = [];
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
      let doc_uid = GState.new_doc_uid () in
      let file_doc = {
        doc_parent ;
        doc_uid ;
        doc_basename = basename ;
        doc_name = doc_parent.folder_name // basename;
        doc_other_names = [];
        doc_tags = StringSet.empty ;
        doc_file = None ;
      } in
      doc_parent.folder_docs <-
        StringMap.add basename file_doc
          doc_parent.folder_docs;
      file_doc

let add_file_classifier f =
  let old_f = !GState.file_classifier in
  GState.file_classifier := (fun ~file_doc ->
      match f ~file_doc with
      | Some file_kind -> Some file_kind
      | None -> old_f ~file_doc)

let profile_append ( profile_var, profile_option ) =
  profile_var := !profile_var @ !!profile_option ;
  profile_option =:= []

let eprint_config () =
  Printf.eprintf "Engine.config:\n%!";
  Printf.eprintf "  * Active warnings: %d\n%!"
    (StringMap.cardinal !GState.active_warnings);
  Printf.eprintf "  * Active linters: %d\n%!"
    (List.length !GState.active_linters);
  List.iter (fun linter ->
      Printf.eprintf "    * %s\n%!" linter.linter_name )
    !GState.active_linters ;
  ()


let add_folder_updater f =
  let old_f = !GState.folder_updater in
  GState.folder_updater := (fun ~folder ->
      old_f ~folder ;
      f ~folder)

let compare_check_start (_,loc1,_) (_,loc2,_) =
  compare loc1.loc_start.pos_cnum
    loc2.loc_start.pos_cnum

let compare_message_start m1 m2 =
  compare
    m1.msg_loc.loc_start.pos_cnum
    m2.msg_loc.loc_start.pos_cnum

let compare_zone_pos m1 m2 =
  compare m1.zone_loc.loc_start.pos_cnum
    m2.zone_loc.loc_start.pos_cnum

let apply_zone z revert_warning_changes =
  let revert_warning_changes = ref revert_warning_changes in
  Parse_spec.parse_spec z.zone_spec
    (fun new_state w ->
       let old_state = w.w_state in
       if not (StringMap.mem w.w_idstr !revert_warning_changes) then begin
         revert_warning_changes :=
           StringMap.add w.w_idstr (w, old_state) !revert_warning_changes ;
       end;
       match new_state, old_state with
       | Warning_disabled, Warning_enabled ->
           w.w_state <- Warning_sleeping ;
           z.zone_rev_changes <- (w, old_state) :: z.zone_rev_changes
       | Warning_disabled, (Warning_disabled | Warning_sleeping) -> ()
       | Warning_enabled, Warning_enabled -> ()
       | Warning_enabled, Warning_sleeping ->
           w.w_state <- Warning_enabled ;
           z.zone_rev_changes <- (w, old_state) :: z.zone_rev_changes
       | Warning_enabled, Warning_disabled ->
           (* TODO: we should create warnings for these ones ! *)
           Printf.eprintf
             "Warning: cannot wake up locally warnings that have been \
              disabled globally (should be sleeping with '?')\n%!"
       | Warning_sleeping, _ ->
           Printf.eprintf
             "Warning: sleeping mode '?' has no meaning in local \
              annotations\n%!"
    );
  !revert_warning_changes

let filter_target_messages target =

  let messages = Array.of_list target.target_messages in
  Array.sort compare_message_start messages;
  let messages = Array.to_list messages in

  let zones = Array.of_list target.target_zones in
  Array.sort compare_zone_pos zones ;
  let zones = Array.to_list zones in

  let checks = Array.of_list target.target_checks in
  Array.sort compare_check_start checks ;
  let checks = Array.to_list checks in

  let rec iter ~messages zones kept_messages revert_warning_changes =
    match messages with
    | [] ->
        StringMap.iter (fun _ (w,state) -> w.w_state <- state)
          revert_warning_changes ;
        List.rev kept_messages
    | m :: rem_messages ->
        match zones with
        | [] ->
            let kept_messages =
              match m.msg_warning.w_state with
              | Warning_enabled ->
                  m :: kept_messages
              | Warning_disabled
              | Warning_sleeping ->
                  kept_messages
            in
            iter ~messages:rem_messages zones kept_messages
              revert_warning_changes
        | z :: rem_zones ->
            if m.msg_loc.loc_start.pos_cnum <
               z.zone_loc.loc_start.pos_cnum then
              let kept_messages =
                match m.msg_warning.w_state with
                | Warning_enabled ->
                    m :: kept_messages
                | Warning_disabled
                | Warning_sleeping ->
                    kept_messages
              in
              iter
                ~messages:rem_messages zones
                kept_messages revert_warning_changes
            else
              match z.zone_rev_zone with
              | Some z ->
                  List.iter (fun (w,state) -> w.w_state <- state)
                    z.zone_rev_changes ;
                  iter
                    ~messages rem_zones kept_messages
                    revert_warning_changes
              | None ->
                  let revert_warning_changes =
                    apply_zone z revert_warning_changes
                  in
                  iter
                    ~messages rem_zones
                    kept_messages revert_warning_changes
  in
  let messages = iter ~messages zones [] StringMap.empty in

  let parse_check ~loc spec f =
    match
      let ns_name, w_num = EzString.cut_at spec '+' in
      let w_num = int_of_string w_num in
      let ns = Hashtbl.find GState.all_namespaces ns_name in
      ns, w_num
    with
    | (ns, w_num) -> f ~loc ns w_num
    | exception exn ->
        eprintf ~loc
          "check yalo annotation raised %s\n%!"
          (Printexc.to_string exn)
  in
  match checks with
  | [] -> messages
  | _ ->
      let rec iter checks ~messages rev_messages =
        match checks, messages with
        | [], _ -> List.rev rev_messages @ messages
        | (spec, loc, _after) :: checks, [] ->
            if spec <> "" then
              parse_check ~loc spec (fun ~loc _ns _w_num ->
                  eprintf ~loc
                    "Check yalo annotation %s FAILED with no warning\n%!"
                    spec
                );
            iter checks ~messages []
        | ("", loc, _after) :: checks, m :: _ ->
            if loc.loc_start.pos_cnum >= m.msg_loc.loc_start.pos_cnum
            then begin
              eprintf ~loc "Check yalo annotation no-warning FAILED\n%!";
              let w = m.msg_warning in
              eprintf
                "   found warning %s+%d at %s:%d\n%!"
                w.w_namespace.ns_name w.w_num
                m.msg_loc.loc_start.pos_fname
                m.msg_loc.loc_start.pos_lnum;
            end (* else
                   begin
                   eprintf "Check yalo annotation no-warning OK at %s:%d\n%!"
                   loc.loc_start.pos_fname
                   loc.loc_start.pos_lnum ;
                   end *);
            iter checks ~messages rev_messages
        | (spec, loc, after) :: checks, m :: messages ->
            parse_check ~loc spec (fun ~loc ns w_num ->
                let w = m.msg_warning in
                if w.w_num = w_num &&
                   w.w_namespace == ns &&
                   (not after ||
                    loc.loc_start.pos_cnum >= m.msg_loc.loc_start.pos_cnum)
                then begin
                  (*
                    eprintf "Check yalo annotation %s OK at %s:%d\n%!"
                    spec
                    loc.loc_start.pos_fname
                    loc.loc_start.pos_lnum ;
                    eprintf "   found warning at %s:%d\n%!"
                    m.msg_loc.loc_start.pos_fname
                    m.msg_loc.loc_start.pos_lnum
                   *)
                  ()
                end
                else
                  begin
                    eprintf ~loc "Check yalo annotation %s FAILED\n%!"
                      spec ;
                    eprintf
                      "   mismatch with warning %s+%d at %s:%d\n%!"
                      w.w_namespace.ns_name w.w_num
                      m.msg_loc.loc_start.pos_fname
                      m.msg_loc.loc_start.pos_lnum
                  end
              );
            iter checks ~messages rev_messages
      in
      let messages = iter checks ~messages [] in
      messages


let get_messages () =
  let messages = ref [] in
  Hashtbl.iter (fun _ target ->
      (* We don't print message on non-existant files *)
      if Sys.file_exists target.target_name then begin
        let target_messages = filter_target_messages target in
        messages := target_messages @ !messages ;

        (* clean everything *)
        target.target_zones <- [] ;
        target.target_checks <- [] ;
        target.target_messages <- target_messages ;

      end;
    )
    GState.all_targets ;
  !messages

let temporary_set_option option value =
  let prev_value = Config.get_simple_option option in
  Config.set_simple_option option value ;
  GState.restore_after_file_lint :=
    (fun () -> Config.set_simple_option option prev_value)
    :: !GState.restore_after_file_lint
