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

open Ezcmd.V2
open Yalo.Config.OP

let arg_specs = Args.[
    [], EZCMD.Anons (fun list ->
            arg_explicit_files := list),
    EZCMD.info ~docv:"FILES" "List of files that should be explicitely linted. You can also use --source-dir DIR (for .ml/.mli files) or --build-dir DIR (for .cmi/.cmt/.cmti fles) to automatically find files to lint in a project. Specifying . here is a shortcut for --source-dir . --build-dir _build";

    ["build-dir"], EZCMD.String (fun s ->
                       arg_build_directories := !arg_build_directories @ [s]
                     ),
    EZCMD.info ~docv:"DIR" "Recurse in build directory DIR, looking for .cmt/.cmti files";

    ["source-dir"], EZCMD.String (fun s ->
                        arg_source_directories := !arg_source_directories @ [s]
                      ),
    EZCMD.info ~docv:"DIR" "Recurse in source directory DIR, looking for .ml/.mli files";

    [ "skip-config-warnings" ], EZCMD.Set arg_skip_config_warnings,
    EZCMD.info "Skip warnings and errors settings by config file";

    [ "profile" ], EZCMD.String (fun s -> arg_profile := Some s),
    EZCMD.info ~docv:"FILE" "Read warnings+errors profile from FILE";

    [ "w" ; "warnings" ], EZCMD.String (fun s -> arg_warnings := !arg_warnings @ [s]),
    EZCMD.info ~docv:"SPEC"
      "Set warnings according to SPEC-ification";

    [ "e" ; "errors" ], EZCMD.String (fun s -> arg_errors := !arg_errors @ [s]),
    EZCMD.info ~docv:"SPEC"
      "Set errors according to SPEC-ification";

    [ "p" ], EZCMD.String (fun s -> arg_projects := !arg_projects @ [ s ]),
    EZCMD.info ~docv:"PROJECT" "Lint only files from PROJECT";
  ]

let cmd command_name =

  let args =
    arg_specs
    @ Args.initial_arg_specs
    @ Args.common_arg_specs
    @ !Yalo.Engine.all_plugins_args
  in

  EZCMD.sub
    command_name
    ~args
    ~doc: "Lint a project or a list of files."
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
          `P ""
        ];
    ]
    (fun () ->

      begin
        match !Args.arg_profile with
        | None -> ()
        | Some filename ->
           Yalo.Config.config_warnings =:= [];
           Yalo.Config.config_errors =:= [];
           Yalo.Config.append filename
      end;

      Yalo.Lint_project.activate_warnings_and_linters
        ~skip_config_warnings: !Args.arg_skip_config_warnings
        (!Args.arg_warnings, !Args.arg_errors);

      begin
        match !Args.arg_explicit_files,
              !Args.arg_source_directories,
              !Args.arg_build_directories with
        | [], [], [] ->
           Args.arg_source_directories := [ "." ];
           Args.arg_build_directories := [ "_build/default" ];
        | [ "." ], _, _ ->
           Args.arg_explicit_files := [] ;
           Args.arg_source_directories := "." :: !Args.arg_source_directories;
           Args.arg_build_directories := "_build/default" :: !Args.arg_build_directories
        | _ -> ()
      end;

      begin match !Args.arg_save_config with
      | None -> ()
      | Some filename ->
         Yalo.Config.save filename
      end;

      if !Args.arg_print_config then
        Print_config.eprint ();

      Yalo.Lint_project.main
        ~explicit_files: !Args.arg_explicit_files
        ~map_src_projects: !Args.arg_map_src_projects
        ~source_directories: !Args.arg_source_directories
        ~build_directories: !Args.arg_build_directories
        ~projects: !Args.arg_projects
        ();

    )
