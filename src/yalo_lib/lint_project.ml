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

open Types
open Yalo_misc.Infix

(* TODO: explicit files and -p PROJECT should be forbidden to appear
   together *)

type file_kind =
  | FOLDER
  | DOCUMENT
  | OTHER
  | INEXISTENT

let file_kind filename =
  match (Unix.lstat filename).st_kind with
  | Unix.S_DIR -> FOLDER
  | Unix.S_REG -> DOCUMENT
  | _ -> OTHER
  | exception _ -> INEXISTENT

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
       let q = Queue.create () in
       (* We would like to extend the list of paths for siblings
          (.ml -> .cmt/.cmi) but 'dune' breaks the relationship
          between source files and corresponding artefacts, so
          we cannot do that. *)
       List.iter (fun filepath ->
           Queue.add filepath q;
         ) paths ;

       while not ( Queue.is_empty q ) do
         let filepath = Queue.take q in
         let fullname = String.concat "/" filepath in
         if Engine.verbose 2 then
           Printf.eprintf "add scanning root %s\n%!" fullname ;
         let rec iter folder path =
           match path with
           | [] ->
              folder.folder_scan <- Scan_forced
           | basename :: path ->
              let file_name = folder.folder_name // basename in
              match file_kind file_name with
              | OTHER ->
                 Printf.eprintf
                   "Configuration error: while scanning %s, %s\n%!"
                   (Yalo_misc.Utils.filename_of_path filepath)
                   "cannot cross links and other special files";
                 exit 2;
              | FOLDER ->
                 let folder = Engine.get_folder folder basename in
                 iter folder path
              | INEXISTENT ->
                 Printf.eprintf "File argument %S does not exist\n%!"
                   fullname;
                 exit 2
              | DOCUMENT ->
                 match path with
                 | _ :: _ ->
                    Printf.eprintf
                      "Configuration error: path %S is not a folder\n%!"
                      basename;
                    exit 2
                 | [] ->
                    let _doc = Engine.get_document folder  basename in
                    ()
         in
         iter fs.fs_folder filepath
       done
  end;

  let matcher = Regexps.MATCHER.create
                  ~exact:true
                  (!GState.profiles_fileattrs @ !!Config.fileattrs ) in
  let get_fileattrs file_name =
    match Regexps.MATCHER.find_all matcher file_name with
    | None -> []
    | Some (_,_,fileattrs) ->
       fileattrs
  in

  (* Because we use a queue, we explore folders breath-first, so that
     we have the guarrantee that at depth N, we have already scanned
     all folders at depth N-1. *)
  let folders_queue = Queue.create () in
  let default_project = Engine.new_project !!Config.project in
  fs.fs_folder.folder_projects <- Engine.project_map_add default_project ;
  Queue.add fs.fs_folder folders_queue ;

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

    !GState.folder_updater ~folder ;

    let attrs = get_fileattrs folder.folder_name in
    List.iter (function attrs ->
                 List.iter (function
                     | Project projects ->
                        if Engine.verbose 2 then
                          Printf.eprintf "   Projects %S\n%!"
                            ( String.concat ":" projects ) ;
                        folder.folder_projects <- StringMap.empty ;
                        List.iter (fun project_name ->
                            folder.folder_projects <-
                              Engine.project_map_add
                                (Engine.new_project project_name)
                                ~map:folder.folder_projects)
                          projects
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
                        folder.folder_tags <-
                          StringSet.add tagname folder.folder_tags
                   ) attrs ;
      ) attrs ;

    let add_files =
      match folder.folder_scan with
      | Scan_disabled ->
         if Engine.verbose 2 then Printf.eprintf " -> Scan_disabled\n%!";
         StringSet.empty
      | Scan_forced ->
         if Engine.verbose 2 then Printf.eprintf " -> Scan_forced\n%!";
         read_folder folder
      | Scan_maybe ->
         if Engine.verbose 2 then Printf.eprintf " -> Scan_maybe\n%!";
         let set = read_folder folder in
         if StringSet.mem ".git" set || StringSet.mem ".yaloskip" set then
           begin
             if Engine.verbose 2 then Printf.eprintf " -> Scan_avoid\n%!";
             StringSet.empty
           end else
           set
    in

    StringSet.iter (fun basename ->
        let file_name = folder.folder_name // basename in
        match file_kind file_name with
        | FOLDER ->
           let subfolder = Engine.get_folder folder basename in
           subfolder.folder_projects <- folder.folder_projects ;
           subfolder.folder_tags <- folder.folder_tags ;

           begin
             match subfolder.folder_scan with
             | Scan_disabled ->
                subfolder.folder_scan <- Scan_maybe
             | _ -> ()
           end
        | DOCUMENT ->
           let doc = Engine.get_document folder basename in
           doc.doc_tags <- folder.folder_tags ;
           ()
        | OTHER -> ()
        | INEXISTENT -> assert false
      ) add_files ;

    StringMap.iter (fun _ subfolder ->
        Queue.add subfolder folders_queue
      ) folder.folder_folders ;

    StringMap.iter (fun _ file_doc ->
        let attrs = get_fileattrs file_doc.doc_name in
        List.iter
          (function attrs ->
             List.iter (function
                 | Project projects ->
                    if Engine.verbose 2 then
                      Printf.eprintf "   Skipping Project %S\n%!"
                        (String.concat ":" projects) ;
                 | Skipdir skipdir ->
                    if Engine.verbose 2 then
                      Printf.eprintf "   Skipping Skipdir %b\n%!" skipdir ;
                 | Tag tagname ->
                    if Engine.verbose 2 then
                      Printf.eprintf "   Tag %S\n%!" tagname ;
                    file_doc.doc_tags <- StringSet.add tagname file_doc.doc_tags
               ) attrs ;
          ) attrs ;
        Engine.add_file ~file_doc

      ) folder.folder_docs ;

  done ;

  let add_file_project file p =
    p.project_files <- file :: p.project_files
  in

  Hashtbl.iter (fun _ file ->
      StringMap.iter (fun _ p ->
          add_file_project file p) file.file_projects ;
      add_file_project file fs.fs_project ;
    ) GState.all_files ;

  Hashtbl.iter (fun _ p ->
      let files = Array.of_list p.project_files in
      Array.sort compare files;
      p.project_files <- Array.to_list files
    ) GState.all_projects ;

  ()

let lint_projects
      ~fs
      ~paths
      ~projects
      ()=

  let projects_to_lint =
    match projects with
    | [] -> begin
        match paths with
        | _ :: _ ->
           [ fs.fs_project ]
        | [] ->
           match !!Config.default_target with
           | None ->
              StringMap.to_list fs.fs_folder.folder_projects |>
                List.map snd
           | Some name -> [ Engine.new_project name ]
      end
    | list ->
       List.map (fun name ->
           try
             Hashtbl.find GState.all_projects name
           with Not_found ->
             Printf.eprintf
               "Configuration error: project %S does not exist\n%!" name;
             Hashtbl.iter (fun _ p ->
                 Printf.eprintf "  * project %S%s\n%!" p.project_name
                   (match p.project_name with
                   | "_" -> " (all files)"
                   | _ -> "")
               ) GState.all_projects ;
             exit 2
         ) list
  in

  let files_done = ref 0 in
  List.iter (fun l ->
      l.linter_begin ()
    ) !GState.active_linters;

  List.iter (fun p ->
      Printf.eprintf "For project %S\n%!" p.project_name ;
      List.iter (fun file ->
          if not file.file_done then begin
              file.file_done <- true;
              incr files_done ;
              if Engine.verbose 1 then
                Printf.eprintf "* %s\n%!" file.file_name;
              file.file_kind.kind_lint ~file;
              List.iter (fun f -> f ()) !GState.restore_after_file_lint ;
              GState.restore_after_file_lint := []
            end
        ) (
          p.project_files
        ) ;
    ) projects_to_lint ;

  List.iter (fun l ->
      l.linter_end ()
    ) !GState.active_linters;

  Printf.eprintf
    "%d files linted with %d active linters looking for %d warnings\n%!"
    !files_done
    (List.length !GState.active_linters)
    (StringMap.cardinal !GState.active_warnings);
  ()

let main
      ~fs
      ~paths
      ~projects
      ?format
      ?autofix
      ?output
      () =

  scan_projects
    ~fs
    ~paths
    ();

  lint_projects
    ~fs
    ~paths
    ~projects
    ();

  (* TODO: also display cached messages *)
  let messages = Engine.get_messages () in
  Message_format.display_messages ?format ?output messages;

  begin match autofix with
  | None -> ()
  | Some inplace ->
     Autofix.apply ~inplace messages ;
  end;
  ()

let activate_warnings_and_linters
      ?profile
      ?(skip_config_warnings=false)
      (arg_warnings, arg_errors) =

  begin
    match profile with
    | None -> ()
    | Some filename ->
       Config.config_warnings =:= [];
       Config.config_errors =:= [];
       Config.append filename
  end;

  let set_warning new_state w =
    w.w_state <- new_state
  in
  let set_error new_state w =
    begin
      match new_state with
      | Warning_enabled
        | Warning_sleeping -> w.w_level_error <- true
      | Warning_disabled -> w.w_level_error <- false
    end;
    match new_state, w.w_state with
    | (Warning_enabled | Warning_sleeping), Warning_disabled ->
       w.w_state <- new_state
    | _ -> ()
  in

  if not skip_config_warnings then
    Parse_spec.parse_spec_list
      ( !GState.profiles_warnings @ !!Config.config_warnings )
      set_warning ;
  Parse_spec.parse_spec_list arg_warnings set_warning ;

  if not skip_config_warnings then
    Parse_spec.parse_spec_list
      ( !GState.profiles_errors @ !!Config.config_errors ) set_error ;
  Parse_spec.parse_spec_list arg_errors set_error ;

  Engine.activate_linters ();

  if List.length !GState.active_linters = 0 then begin
      Printf.eprintf "Configuration error: no active linters\n%!";
      Engine.eprint_config ();
      exit 2
    end;
  if Engine.verbose 1 then begin
      Engine.eprint_config ();
    end
