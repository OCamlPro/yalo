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

(* TODO: explicit files and -p PROJECT should be forbidden to appear
   together *)

let scan_projects
      ~fs
      ~paths
      ()
  =

  begin
    match paths with
    | [] ->
       fs.fs_folder.folder_scan <- Scan_forced
    | paths ->
       List.iter (fun path ->
           let rec iter folder path =
             match path with
             | [] ->
                folder.folder_scan <- Scan_forced
             | basename :: path ->
                let file_name = folder.folder_name // basename in
                if Sys.is_directory file_name then
                  let folder = Engine.get_folder folder basename in
                  iter folder path
                else
                  match path with
                  | _ :: _ ->
                     Printf.eprintf "Configuration error: path %S is not a folder\n%!" basename;
                     exit 2
                  | [] ->
                     let _doc = Engine.get_document folder  basename in
                     ()
           in
           iter fs.fs_folder path
         ) paths
  end;


  let matcher = Regexps.MATCHER.create
                  ~exact:true
                  (!Engine.profiles_fileattrs @ !!Config.fileattrs ) in
  let get_fileattrs file_name =
    match Regexps.MATCHER.find_all matcher file_name with
    | None -> []
    | Some (_,_,fileattrs) -> fileattrs
  in

  let folders_queue = Queue.create () in
  let default_project = Engine.new_project !!Config.default_project in
  fs.fs_folder.folder_project <- default_project ;
  Queue.add fs.fs_folder folders_queue ;

  (*
    let add_file_to_lint ~file_folder ?(error=false) ~build ~source ?p
    file_name =

    let fileattrs = match Regexps.MATCHER.find_all matcher file_name with
    | None -> []
    | Some (_,_,fileattrs) -> fileattrs
    in
    let file_tags = ref StringSet.empty in
    let file_tags = !file_tags in
    Engine.add_file ~file_folder ~file_tags file_name ?p
    in
   *)
  
  (* Some rules:
   * at this point, all explicit_files are subnames of the current
   directory. If none, we need to add "".

   * if we find a .yaloconf file inside a directory that we are
   planning to add, we skip the corresponding directory. If it is
   explicit, we error.

   * we scan all other files and directories
   *)

  let read_folder folder =
    let files = Sys.readdir (match folder.folder_name with
                  | "" -> "."
                  | name -> name) in
    let set = ref StringSet.empty in
    Array.iter (fun basename ->
        set := StringSet.add basename !set) files ;
    !set
  in

  while not (Queue.is_empty folders_queue) do
    let folder = Queue.take folders_queue in

    if Engine.verbose 2 then
      Printf.eprintf "Checking folder %S\n%!" folder.folder_name ;

    let attrs = get_fileattrs folder.folder_name in
    List.iter (function attrs ->
                 List.iter (function
                     | Project project_name ->
                        if Engine.verbose 2 then
                          Printf.eprintf "   Project %S\n%!" project_name ;
                        folder.folder_project <- Engine.new_project project_name
                     | Skipdir skipdir ->
                        if Engine.verbose 2 then
                          Printf.eprintf "   Skipdir %b\n%!" skipdir ;
                        begin
                          match folder.folder_scan, skipdir with
                          | Scan_maybe, true ->
                             folder.folder_scan <- Scan_disabled
                          | _ -> ()
                        end
                     | Tag tagname ->
                        if Engine.verbose 2 then
                          Printf.eprintf "   Tag %S\n%!" tagname ;
                        folder.folder_tags <- StringSet.add tagname folder.folder_tags
                   ) attrs ;
      ) attrs ;

    let add_files =
      match folder.folder_scan with
      | Scan_disabled -> StringSet.empty
      | Scan_forced -> read_folder folder
      | Scan_maybe ->
         let set = read_folder folder in
         if StringSet.mem Constant.config_basename set
            || StringSet.mem ".git" set then
           StringSet.empty
         else
           set
    in

    StringSet.iter (fun basename ->
        let file_name = folder.folder_name // basename in
        if Sys.is_directory file_name then
          let subfolder = Engine.get_folder folder basename in
          subfolder.folder_project <- folder.folder_project ;
          subfolder.folder_tags <- folder.folder_tags ;

          begin
            match subfolder.folder_scan with
            | Scan_disabled ->
               subfolder.folder_scan <- Scan_maybe
            | _ -> ()
          end
        else
          let doc = Engine.get_document folder basename in
          doc.doc_tags <- folder.folder_tags ;
          ()
      ) add_files ;

    StringMap.iter (fun _ subfolder ->
        Queue.add subfolder folders_queue
      ) folder.folder_folders ;

    StringMap.iter (fun _ file_doc ->
        let attrs = get_fileattrs file_doc.doc_name in
        List.iter
          (function attrs ->
             List.iter (function
                 | Project project_name ->
                    if Engine.verbose 2 then
                      Printf.eprintf "   Skipping Project %S\n%!" project_name ;
                 | Skipdir skipdir ->
                    if Engine.verbose 2 then
                      Printf.eprintf "   Skipping Skipdir %b\n%!" skipdir ;
                 | Tag tagname ->
                    if Engine.verbose 2 then
                      Printf.eprintf "   Tag %S\n%!" tagname ;
                    file_doc.doc_tags <- StringSet.add tagname file_doc.doc_tags
               ) attrs ;
          ) attrs ;
        Engine.add_file ~file_doc ()

      ) folder.folder_docs ;

  done ;

  let add_file_project file p =
    p.project_files <- file :: p.project_files
  in

  Hashtbl.iter (fun _ file ->
      add_file_project file file.file_project ;
      add_file_project file fs.fs_project ;
    ) Engine.all_files ;

  Hashtbl.iter (fun _ p ->
      let files = Array.of_list p.project_files in
      Array.sort compare files;
      p.project_files <- Array.to_list files
    ) Engine.all_projects ;

  ()

let lint_projects
      ~fs
      ~projects
      ()=

  let projects_to_lint =
    match projects with
    | [] -> [ fs.fs_folder.folder_project ]
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
              file.file_kind.kind_lint ~file
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
      ~fs
      ~paths
      ~projects
      () =

  scan_projects
    ~fs
    ~paths
    ();

  lint_projects
    ~fs
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

