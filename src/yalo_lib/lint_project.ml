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

(* TODO: we need a way to disable warnings locally in files.
   Typically, if a warning is raised and the loc is withing a range
   where the warning is disabled.
   For sources, we could use the line-linter for that, i.e.
   (* YALO-FILE: PLUGIN-12 *)
   (* YALO-SCOPE-BEGIN: PLUGIN-23 *)
   [...]
   (* YALO-SCOPE-END *)

   Note: is it possible to activate a warning in a scope ? Maybe we
   need to have another level, to still look for a warning, but not
   display it, unless asked for.

   Also, we may want in the .yalocaml file to select which warnings
   are activated on a per-file basis. But don't forget that command
   line arguments take precedence over config files.
 *)

open EzCompat

open Ez_file.V1
open Types
open EzFile.OP
open Config.OP

let display_messages () =
  match Engine.get_messages () with
  | [] -> ()
  | messages ->
     let nwarnings = ref 0 in
     let nerrors = ref 0 in
     List.iter (fun m ->
         if m.msg_warning.w_level_error then
           incr nerrors
         else
           incr nwarnings ;
       ) messages ;
     begin
       match !nwarnings, !nerrors with
       | 0, n ->
          Printf.eprintf "Yalo: %d errors found\n" n
       | n, 0 ->
          Printf.eprintf "Yalo: %d warnings found\n" n
       | nw, ne ->
          Printf.eprintf "Yalo: %d errors and %d warnings found\n" ne nw
     end;
     List.iter (fun m ->
         Location.print_loc Format.str_formatter m.msg_loc;
         let loc = Format.flush_str_formatter () in
         Printf.eprintf "%s\n%!" loc;
         Printf.eprintf "%s (%s): %s\n%!"
           (if m.msg_warning.w_level_error then
             "Error"
           else
             "Warning")
           m.msg_warning.w_idstr
           m.msg_string;
       (* TODO: display context ? *)
       ) messages ;
     if !nerrors > 0 then exit 2

let new_project name =
  match Hashtbl.find Engine.all_projects name with
  | p -> p
  | exception Not_found ->
     let p = {
         project_name = name ;
         project_files = [] ;
       } in
     Hashtbl.add Engine.all_projects name p ;
     p

let scan_projects
      ~project_all
      ~map_src_projects
      ~explicit_files
      ~source_directories
      ~build_directories
      ()
  =

  let add_file_to_lint ?(error=false) ~build ~source ?p file =
    if source && Filename.check_suffix file ".ml" then
      Engine.add_file file ?p
    else
      if source && Filename.check_suffix file ".mli" then
        Engine.add_file file ?p
      else
        if build && Filename.check_suffix file ".cmt" then
          Engine.add_file file ?p
        else
          if build && Filename.check_suffix file ".cmti" then
            Engine.add_file file ?p
          else
            (* TODO : cmi files ? *)
            if error then
              begin
                Printf.eprintf "Error: don't know what to do with %s\n%!" file;
                exit 2
              end
  in

  (* TODO: the .yalo-project is not copied into the _build/default
     directory. We may need to remember paths indicated by
     .yalo-project files and use them when scanning build dirs. *)

  (* TODO: this is wrong, this file should be loaded only
     on source-directories *)
  let default_project_name =
    if Sys.file_exists Constant.project_basename then
      match Yalo_project.read Constant.project_basename with
      | { pr_project = Some name ; _ } -> name
      | _ -> "."
    else
      "." (* TODO document *)
  in

  List.iter (add_file_to_lint
               ~error:true
               ~build:true
               ~source:true) explicit_files;

  (* TODO: instead of this, we will use a list of projects in the
     .yalocaml file, specifying which dirs belong to which
     projects. *)
  let map_src = ref StringMap.empty in

  let scan_directory ~is_build dir =
    let is_source = not is_build in
    let dirs = Queue.create () in
    Queue.add (None, "") dirs;
    while not (Queue.is_empty dirs) do
      let p, subdir = Queue.take dirs in
      let files = Sys.readdir (dir // subdir) in
      Array.sort compare files;
      Array.iter (fun basename ->
          let subfile = subdir // basename in
          let file = dir // subfile in
          if Sys.is_directory file then
            match basename.[0] with
            (* always skip directories starting with _ *)
            | '_' -> ()
            (* skip directories starting with . only for sources, as
               dune stores object files into such directories *)
            | '.' when is_source -> ()
            | _ ->
               let p =
                 let yalo_project_file =
                   file // Constant.project_basename
                 in
                 if Sys.file_exists yalo_project_file then
                   let ypr = Yalo_project.read yalo_project_file in

                   match ypr.pr_project with
                   | None -> p
                   | Some name ->

                      if is_source && map_src_projects then
                        map_src := StringMap.add subfile name !map_src;
                      match p with
                      | None when name = default_project_name ->
                         None
                      | _ -> Some ( new_project name )
                      else
                        if is_build && map_src_projects then
                          match StringMap.find subfile !map_src with
                          | exception Not_found -> p
                          | name ->
                             match p with
                             | None when name = default_project_name ->
                                None
                             | _ -> Some ( new_project name )
                        else
                          p
               in
               Queue.add (p, subfile) dirs
          else
            add_file_to_lint
              ~build:is_build
              ~source:is_source
              ?p file
        ) files
    done
  in

  List.iter (scan_directory ~is_build:false) source_directories ;
  List.iter (scan_directory ~is_build:true) build_directories ;

  let project_default = new_project default_project_name in

  let add_file_project file p =
    p.project_files <- file :: p.project_files
  in

  Hashtbl.iter (fun _ file ->
      if file.file_projects = StringMap.empty then
        file.file_projects <-
          StringMap.add
            project_default.project_name project_default
            file.file_projects ;
      add_file_project file project_all ;
      StringMap.iter (fun _ p ->
          add_file_project file p) file.file_projects
    ) Engine.all_files ;

  Hashtbl.iter (fun _ p ->
      let files = Array.of_list p.project_files in
      Array.sort compare files;
      p.project_files <- Array.to_list files
    ) Engine.all_projects ;

  ()

let lint_projects
      ~project_all
      ~projects
      ()=

  let projects_to_lint =
    match projects with
    | [] -> [ project_all ]
    | list ->
       List.map (fun name ->
           try
             Hashtbl.find Engine.all_projects name
           with Not_found ->
             Printf.eprintf "Configuration error: project %S does not exist\n%!" name;
             Hashtbl.iter (fun _ p ->
                 Printf.eprintf "  * project %S%s\n%!" p.project_name
                   (match p.project_name with
                   | "_" -> " (all files)"
                   | "." -> " (default project)"
                   | _ -> "")
               ) Engine.all_projects ;
             exit 2
         ) list
  in

  List.iter (fun l ->
      l.linter_begin ()
    ) !Engine.active_linters;

  List.iter (fun p ->
      Printf.eprintf "For project %S\n%!" p.project_name ;
      List.iter (fun file ->
          if not file.file_done then begin
              file.file_done <- true;
              file.file_kind.kind_checker ~file
            end
        ) (
          p.project_files
        ) ;
    ) projects_to_lint ;

  List.iter (fun l ->
      l.linter_end ()
    ) !Engine.active_linters;
  ()

let main
      ~explicit_files
      ~map_src_projects
      ~source_directories
      ~build_directories
      ~projects
      () =

  let project_all = new_project "_" in (* TODO Document *)

  scan_projects
    ~project_all
    ~explicit_files
    ~map_src_projects
    ~source_directories
    ~build_directories
    ();

  lint_projects
    ~project_all
    ~projects
    ();

  (* TODO: also display cached messages *)
  display_messages ();

  (* val display_messages : unit -> unit *)
  ()

let activate_warnings_and_linters
      ?(skip_config_warnings=false) (arg_warnings, arg_errors) =

  let set_warning set w = w.w_level_warning <- set in
  let set_error set w = w.w_level_error <- set in

  if not skip_config_warnings then
    Parse_spec.parse_spec_list !!Config.config_warnings set_warning ;
  Parse_spec.parse_spec_list arg_warnings set_warning ;

  if not skip_config_warnings then
    Parse_spec.parse_spec_list !!Config.config_errors set_error ;
  Parse_spec.parse_spec_list arg_errors set_error ;

  Engine.activate_linters ();

