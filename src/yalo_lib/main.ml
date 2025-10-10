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

let load_plugins () =

  List.iter (fun arg ->
      Printf.eprintf "Loading %s\n%!" arg;
      let file =
        if Filename.check_suffix arg ".cmxs" ||
             Filename.check_suffix arg ".cmx" ||
               Filename.check_suffix arg ".cmo"
        then
          arg
        else
          if Filename.check_suffix arg ".ml" then
            arg
          else
            arg ^
              (if Sys.backend_type = Native then
                ".cmxs"
              else
                ".cmo")
      in
      let file = try
          Utils.find_in_path !Args.arg_load_dirs file
        with Not_found ->
          try
            let is_plugin = ref true in
            for i = 0 to String.length arg - 1 do
              match arg.[i] with
              | '.' | '/' | '\\' -> is_plugin := false
              | _ -> ()
            done;
            if !is_plugin then
              let refdir = Filename.dirname Sys.executable_name in
              let file = refdir //
                           Printf.sprintf "../lib/%s/%s.cmxs"
                             arg arg
              in
              if Sys.file_exists file then
                file
              else
                raise Not_found
            else
              raise Not_found
          with Not_found ->
            Printf.eprintf "Error: could not find %S in:\n%!" arg;
            List.iter (fun dir ->
                Printf.printf "  - %s\n%!" dir;
              ) !Args.arg_load_dirs;
            exit 2
      in
      if Filename.check_suffix file ".ml" then begin
          let source = EzFile.read_file file in
          let crc = Digest.string source in
          let crc = Digest.to_hex crc in
          (* TODO: create _yalo where .yalocaml is *)
          let prefix = Printf.sprintf "%s/Yalo_%s"
                         Constant.temp_dir crc in
          (* TODO: bytecode version *)
          let file_obj = prefix ^ ".cmxs" in
          let file_ml = prefix ^ ".ml" in
          try
            if Sys.file_exists file_obj then
              try
                Dynlink.loadfile file_obj
              with exn ->
                Printf.eprintf "Loading %s failed. Rebuilding plugin\n%!" file_obj;
                Sys.remove file_obj;
                raise exn
            else raise Not_found
          with _ ->
                if not ( Sys.file_exists Constant.temp_dir ) then
                  Unix.mkdir Constant.temp_dir 0o755;
                EzFile.write_file file_ml source;
                (* TODO: bytecode version *)
                let cmd = Printf.sprintf
                            "ocamlopt -shared -opaque -I %s -o %s %s"
                            (String.concat " -I " !Args.arg_load_dirs)
                            file_obj file_ml
                in
                Printf.eprintf "Call: %s\n%!" cmd;
                let ret = Sys.command cmd in
                if ret <> 0 then begin
                    Printf.eprintf "Error: could not compile %s\n%!" arg;
                    exit 2
                  end;
                Dynlink.loadfile file_obj
        end
      else
        Dynlink.loadfile file
    ) !Args.arg_load_plugins;

  Printf.eprintf "load plugins done\n%!"

let real_common_init () =
  begin
    try ignore ( Sys.getcwd () )
    with _ ->
      Printf.eprintf "Current directory does not exist anymore. Move back up.\n%!";
      exit 2
  end ;
  Printexc.record_backtrace true;

  let config_file =
    match !Args.arg_config_file with
    | Some file -> Some file
    | None ->
       try
         (* TODO we may want to load the .yalocaml, starting from
            the file what we are supposed to parse. But we only
            see it at the end ? We could add a
            -T <target-file> early arg for that. *)
         let file = Utils.find_file Constant.config_basename in
         let dir = Filename.dirname file in
         Args.arg_load_dirs := dir :: !Args.arg_load_dirs ;
         Some file
       with Not_found -> None
  in

  begin
    match config_file with
    | None ->
       Printf.eprintf "Warning: no file %s found. Using default config.\n%!" Constant.config_basename;
       ()
    | Some file ->
       Config.load file
  end;

  Args.arg_load_dirs := !Args.arg_load_dirs @ !!Config.config_load_dirs ;
  
  Args.arg_load_plugins :=
    !!Config.config_load_plugins @ !Args.arg_load_plugins ;

  if not !Args.arg_no_load_plugins then
    load_plugins ();

  ()

let common_inited = ref false
let common_init () =
  if not !common_inited then begin
      common_inited := true ;
      real_common_init ();
    end

let real_final_init () =

  begin
    match !Args.arg_profile with
    | None -> ()
    | Some filename ->
       Config.config_warnings =:= [];
       Config.config_errors =:= [];
       Config.append filename
  end;

  let set_warning set w = w.w_level_warning <- set in
  let set_error set w = w.w_level_error <- set in

  if not !Args.arg_skip_config_warnings then
    Parse_spec.parse_spec_list !!Config.config_warnings set_warning ;
  Parse_spec.parse_spec_list !Args.arg_warnings set_warning ;

  if not !Args.arg_skip_config_warnings then
    Parse_spec.parse_spec_list !!Config.config_errors set_error ;
  Parse_spec.parse_spec_list !Args.arg_errors set_error ;

  Printf.eprintf "Warning: optional activation is not implemented\n%!";
  Engine.activate_linters ();
  ()

let final_inited = ref false
let final_init () =
  if !final_inited then () else
    let () = final_inited := true in
    real_final_init ()




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

let main () =
  let args = Array.to_list Sys.argv in
  let _cmd, args = match args with
      cmd :: args -> cmd, args
    | [] -> assert false
  in

  let args = Args.parse_initial_args args in
  common_init ();
  Args.parse args ;

  Clflags.error_style := Some Misc.Error_style.Contextual;
  Clflags.include_dirs :=
    (List.rev !Args.arg_load_dirs) @ Clflags.include_dirs.contents;

  final_init ();

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
  in

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
               ~source:true) !Args.arg_explicit_files;

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

                      if is_source && !Args.arg_map_src_projects then
                        map_src := StringMap.add subfile name !map_src;
                      match p with
                      | None when name = default_project_name ->
                         None
                      | _ -> Some ( new_project name )
                      else
                        if is_build && !Args.arg_map_src_projects then
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

  List.iter (scan_directory ~is_build:false) !Args.arg_source_directories ;
  List.iter (scan_directory ~is_build:true) !Args.arg_build_directories ;

  let project_default = new_project default_project_name in
  let project_all = new_project "_" in (* TODO Document *)

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

  begin match !Args.arg_save_config with
  | None -> ()
  | Some filename ->
     Config.save filename
  end;
  if !Args.arg_print_config then
    Print_config.eprint ();

  let projects_to_lint =
    match !Args.arg_projects with
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

  (* TODO: also display cached messages *)
  display_messages ();
  ()

let () =
  V1.init (); (* needed to force linking for plugins *)
