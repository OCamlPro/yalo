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

open EzCompat
open Ez_file.V1
open Yalo_misc.Utils.OP
open Config.OP

let load_plugin file =
  try
    Dynlink.loadfile file
  with
    Dynlink.Error (Module_already_loaded _) -> ()


let bin_dir = Sys.executable_name |> Filename.dirname

let in_yalo_sources =
  Sys.file_exists (bin_dir // "src" // "yalo_driver" // "main.ml")

let opam_dir =
  if in_yalo_sources then
    bin_dir // "_build" // "install" // "default"
  else
    bin_dir |> Filename.dirname
let lib_dir = opam_dir // "lib"
let share_dir = opam_dir // "share"
let yalo_share_dir = share_dir // "yalo"
let yalo_profiles_dir = yalo_share_dir // "profiles"

let load_plugins ~plugins () =
  Clflags.error_style := Some Misc.Error_style.Contextual;
  Clflags.include_dirs :=
    !Engine.profiles_load_dirs @ !Clflags.include_dirs;

  List.iter (fun arg ->
      if Engine.verbose 1 then
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
          Yalo_misc.Utils.find_in_path !Clflags.include_dirs file
        with Not_found ->
          try
            let is_plugin = ref true in
            for i = 0 to String.length arg - 1 do
              match arg.[i] with
              | '.' | '/' | '\\' -> is_plugin := false
              | _ -> ()
            done;
            if !is_plugin then
              let plugin_dir = lib_dir // arg in
              let file = plugin_dir // (arg ^ ".cmxs") in
              if Sys.file_exists file then begin
                  Clflags.include_dirs := plugin_dir :: !Clflags.include_dirs;
                file
              end else
                raise Not_found
            else
              raise Not_found
          with Not_found ->
            Printf.eprintf "Error: could not find %S in:\n%!" arg;
            List.iter (fun dir ->
                Printf.printf "  - %s\n%!" dir;
              ) !Clflags.include_dirs;
            exit 2
      in
      if Filename.check_suffix file ".ml" then
        let source = EzFile.read_file file in
        let crc = Digest.string source in
        let crc = Digest.to_hex crc in
        (* TODO: create _yalo where .yalocaml is *)
        let prefix = Printf.sprintf "%s/Yalo_%s"
                       Constant.temp_dir crc in
        (* TODO: bytecode version *)
        let file_obj = prefix ^ ".cmxs" in
        let file_ml = prefix ^ ".ml" in

        let build_file () =
          if not ( Sys.file_exists Constant.temp_dir ) then
            Unix.mkdir Constant.temp_dir 0o755;
          EzFile.write_file file_ml source;
          (* TODO: bytecode version *)
          let cmd = Printf.sprintf
                      "ocamlopt -shared -opaque %s -o %s %s"
                      (let dirs = !Clflags.include_dirs in
                       let yalo_lib_dir = lib_dir // "yalo_lib" in
                       let dirs = dirs @ [ yalo_lib_dir ] in
                       Printf.sprintf "-I '%s'"
                         (String.concat "' -I '" dirs))
                      file_obj file_ml
          in
          Printf.eprintf "Call: %s\n%!" cmd;
          let ret = Sys.command cmd in
          if ret <> 0 then begin
              Printf.eprintf "Error: could not compile %s\n%!" arg;
              exit 2
            end;
        in
        if Sys.file_exists file_obj then
          (* we cannot try to use a precompiled version, instead we need
             to rebuild it everytime because of a bug in Dynlink:

            try
            load_plugin file_obj
            with exn ->
            Printf.eprintf "Loading %s failed (%s). Rebuilding plugin\n%!"
            (Printexc.to_string exn) file_obj;
           *)
          let () = () in
          Sys.remove file_obj;
          build_file ();
          load_plugin file_obj
        else begin
            build_file ();
            load_plugin file_obj
          end
      else
        load_plugin file
    ) plugins;

  ()

let init
      ?config_file
      ?(load_dirs=[])
      ?(plugins=[])
      ?(can_load_plugins=true)
      ?(profiles=[])
      () =

  let fs_root =
    try Sys.getcwd () with _ ->
      Printf.eprintf "Current directory does not exist anymore. Move back up.\n%!";
      exit 2
  in

  let fs_root, fs_subpath, load_dirs, config_file =
    match config_file with
    | Some file -> fs_root, [], load_dirs, Some file
    | None ->
       try
         (* TODO we may want to load the .yalocaml, starting from
            the file what we are supposed to parse. But we only
            see it at the end ? We could add a
            -T <target-file> early arg for that. *)
         let file, subpath = Yalo_misc.Utils.find_file Constant.config_basename in
         let dir = Filename.dirname file in
         Printf.eprintf "yalo: Entering directory '%s'\n%!" dir;
         at_exit (fun () ->
             Printf.eprintf "yalo: Leaving directory '%s'\n%!" dir;
           );
         Sys.chdir dir ;
         let load_dirs =
           "." ::
             List.map (Yalo_misc.Utils.normalize_filename ~subpath)
               load_dirs in
         dir, subpath, load_dirs, Some file
       with Not_found ->
         fs_root, [], load_dirs, None
  in

  Engine.profiles_load_dirs := load_dirs ;
  let fs = Engine.new_fs ~fs_root ~fs_subpath in

  begin
    match config_file with
    | None ->
       Printf.eprintf "Warning: no file %s found. Using default config.\n%!" Constant.config_basename;
       ()
    | Some file ->
       Config.load file ;
       Engine.profiles_load_dirs :=
         !Engine.profiles_load_dirs
         @ !!Config.config_load_dirs
         @ [ yalo_profiles_dir ] ;
       let profiles = ref (!!Config.config_profiles @ profiles) in
       let loaded_profiles = ref StringSet.empty in
       let rec iter () =
         match !profiles with
         | [] -> ()
         | profile :: others ->
            profiles := others ;

            if not @@ StringSet.mem profile !loaded_profiles then
              let basename =
                if Filename.check_suffix profile ".conf" then
                  profile
                else
                  Printf.sprintf "yalo-%s.conf" profile
              in
              let file = try
                  Yalo_misc.Utils.find_in_path
                    !Engine.profiles_load_dirs basename
                with Not_found ->
                  Printf.eprintf
                    "Execution error: profile %S not found in search path\n%!"
                    basename;
                  List.iter (fun s ->
                      Printf.eprintf "  - %S\n%!" s
                    ) !Engine.profiles_load_dirs ;
                  exit 2
              in
              Config.append file ;
              loaded_profiles := StringSet.add profile !loaded_profiles;

              profiles := !profiles @ !!Config.profile_profiles ;

              Engine.profile_append ( Engine.profiles_fileattrs,
                                      Config.profile_fileattrs ) ;

              List.iter Engine.profile_append [

                  Engine.profiles_load_dirs, Config.profile_load_dirs ;
                  Engine.profiles_plugins, Config.profile_plugins ;
                  Engine.profiles_profiles, Config.profile_profiles ;
                  Engine.profiles_warnings, Config.profile_warnings ;
                  Engine.profiles_errors, Config.profile_errors ;

                ];

              iter ()
       in
       iter ()
  end;

  let plugins =
    !Engine.profiles_plugins @ !!Config.config_load_plugins @ plugins in

  if can_load_plugins then
    load_plugins ~plugins ();

  fs
