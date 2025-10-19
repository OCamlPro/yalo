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

      if !Args.arg_print_config then
        Print_config.eprint ();

      let fs = Init.get_fs () in

      let paths =
        List.map (fun filename ->
            Yalo.Utils.path_of_filename ~subpath:fs.fs_subpath filename
          ) !Args.arg_explicit_files
      in

      Yalo.Lint_project.main
        ~fs
        ~paths
        ~projects: !Args.arg_projects
        ();

    )
