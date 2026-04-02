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
    "List of files or directories that should be explicitely linted";

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

  [ "f"; "message-format" ],
  EZCMD.String (function
      | "context" -> Args.arg_message_format := Format_Context
      | "human" -> Args.arg_message_format := Format_Human
      | "short" -> Args.arg_message_format := Format_Short
      | "summary" -> Args.arg_message_format := Format_Summary
      | "sarif" -> Args.arg_message_format := Format_Sarif
      | "codeclimate" -> Args.arg_message_format := Format_CodeClimate
      | "gitlab" -> Args.arg_message_format := Format_GitLab
      (* TODO Clippy: human, short, json, json-diagnostic-short,
         json-diagnostic-rendered-ansi, json-render-diagnostics *)
      | s ->
          Printf.eprintf
            "Configuration error: message format %S does not exist\n%!" s;
          exit 2
    ),
  EZCMD.info ~docv:"FORMAT"
    "Set message format to FORMAT: context (default), human, short, \
     summary, sarif, codeclimate, gitlab";

  [ "p" ; "package" ],
  EZCMD.String (fun s -> Args.arg_projects := !Args.arg_projects @ [ s ]),
  EZCMD.info ~docv:"PROJECT" "Lint only files from PROJECT";
  (* TODO      --all-targets       Check all targets *)

  [ "autofix" ],
  EZCMD.Unit (fun () ->
      match !Args.arg_autofix_inplace with
      | None -> Args.arg_autofix_inplace := Some false
      | Some _ -> ()),
  EZCMD.info "Apply all automatic replacements (files created in _yalo/)" ;

  [ "autofix-inplace" ],
  EZCMD.Unit (fun () -> Args.arg_autofix_inplace := Some true),
  EZCMD.info "Autofix files in place" ;

  [ "o" ; "output" ],
  EZCMD.String (fun s -> Args.arg_output := Some s),
  EZCMD.info ~docv:"FILE" "File for JSON output";

  [ "summary-from" ],
  EZCMD.Int (fun i -> Args.arg_summary := Some i),
  EZCMD.info ~docv:"NUMBER" "Print summary when warnings exceed $(NUMBER)";

  [ "no-summary" ],
  EZCMD.Unit (fun () -> Args.arg_summary := None),
  EZCMD.info "Never print any summary";

]

let cmd command_name =

  let args =
    arg_specs
    @ Args.initial_arg_specs
    @ Args.common_arg_specs
    @ !Yalo.GState.all_plugins_args
  in
  let li a b = `I (a,b) in
  EZCMD.sub
    command_name
    ~args
    ~doc: "Lint a project or a list of files."
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Thie command will perform the following actions";
        `P "Early actions (common to all sub-commands):";
        li "1."
          "Lookup the .yaloconf file in the containing folders. if \
           located, chdir to the corresponding directory." ;
        li "2."
          "If a configuration file was found, load the corresponding \
           file. If profiles are specified in the configuration file, \
           recursively load the profiles too.";
        li "3."
          "If plugins are specified on command line, in the \
           configuration file or in profiles specified in the \
           configuration file, load the plugins";
        `P "Specific actions:";
        li "a."
          "Enable/disable warnings following command line and \
           configuration options. Enable only linters for enabled \
           warnings.";
        li "b."
          "Scan the project tree, looking for files to lint. Each file \
           is associated with a set of including projects.";
        li "c."
          "Lint all the files of selected projects";
        li "d."
          "Display or output warnings";
        li "e."
          "Apply autofix patches if available and the --autofix option \
           was used";
      ];
      `S "INITIAL ARGUMENTS";
      `Blocks [
        `P "Some arguments MUST be specified before the sub-command \
            name (-L,-P,-I,-C,--no-load-plugins). The reason is that \
            these arguments are used to define which and how plugins \
            should be loaded, either directly or though configuration \
            files, and plugins can define new arguments for \
            sub-commands and even new sub-commands"
      ]
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
         ~summary:!Args.arg_summary
         ();

    )
