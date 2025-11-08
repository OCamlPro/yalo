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
open Yalo.Types

let arg_specs = [
    [], EZCMD.Anons (fun list ->
            Args.arg_explicit_files := list),
    EZCMD.info ~docv:"FILES"
      "List of files that should be explicitely linted. You can also use\
       --source-dir DIR (for .ml/.mli files) or --build-dir DIR (for\
       .cmi/.cmt/.cmti fles) to automatically find files to lint in a\
       project.";

    [ "skip-config-warnings" ], EZCMD.Set Args.arg_skip_config_warnings,
    EZCMD.info "Skip warnings and errors settings by config file";

    [ "w" ; "warnings" ],
    EZCMD.String (fun s -> Args.arg_warnings := !Args.arg_warnings @ [s]),
    EZCMD.info ~docv:"SPEC"
      "Set warnings according to SPEC-ification";

    [ "e" ; "errors" ], EZCMD.String
                          (fun s -> Args.arg_errors :=
                                      !Args.arg_errors @ [s]),
    EZCMD.info ~docv:"SPEC"
      "Set errors according to SPEC-ification";

    [ "message-format" ],
    EZCMD.String (function
        | "context" -> Args.arg_message_format := Format_Context
        | "human" -> Args.arg_message_format := Format_Human
        | "sarif" -> Args.arg_message_format := Format_Sarif
        | "short" -> Args.arg_message_format := Format_Short
        (* TODO Clippy: human, short, json, json-diagnostic-short,
           json-diagnostic-rendered-ansi, json-render-diagnostics *)
        | s ->
           Printf.eprintf
             "Configuration error: message format %S does not exist\n%!" s;
           exit 2
      ),
    EZCMD.info ~docv:"FORMAT"
      "Set message format to FORMAT: human, short, sarif(JSON)";

    [ "p" ; "package" ],
    EZCMD.String (fun s -> Args.arg_projects := !Args.arg_projects @ [ s ]),
    EZCMD.info ~docv:"PROJECT" "Lint only files from PROJECT";
    (* TODO      --all-targets       Check all targets *)

    [ "autofix" ],
    EZCMD.Unit (fun () ->
        match !Args.arg_autofix_inplace with
        | None -> Args.arg_autofix_inplace := Some false
        | Some _ -> ()),
    EZCMD.info "Apply all automatic replacements (files created in _YALO/)" ;

    [ "autofix-inplace" ],
    EZCMD.Unit (fun () -> Args.arg_autofix_inplace := Some true),
    EZCMD.info "Autofix files in place" ;

    [ "o" ; "output" ],
    EZCMD.String (fun s -> Args.arg_output := Some s),
    EZCMD.info ~docv:"FILE" "File for JSON output";

  ]

let cmd command_name =

  let args =
    arg_specs
    @ Args.initial_arg_specs
    @ Args.common_arg_specs
    @ !Yalo.GState.all_plugins_args
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

      Yalo.Lint_project.activate_warnings_and_linters
        ~skip_config_warnings: !Args.arg_skip_config_warnings
        (!Args.arg_warnings, !Args.arg_errors);

      if !Args.arg_print_config then
        Print_config.eprint ();

      let fs = Init.get_fs () in

      let path_of_filename =
        Yalo_misc.Utils.path_of_filename ~subpath:fs.fs_subpath
      in
      let normalize_filename =
        Yalo_misc.Utils.normalize_filename ~subpath:fs.fs_subpath
      in
      let paths =
        List.map path_of_filename !Args.arg_explicit_files
      in

      let output = Option.map normalize_filename !Args.arg_output in

      Yalo.Lint_project.main
        ~fs
        ~paths
        ~projects: !Args.arg_projects
        ~format:!Args.arg_message_format
        ?autofix: !Args.arg_autofix_inplace
        ?output
        ();

    )
