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


(* initial arguments *)
let arg_no_load_plugins = ref false
let arg_config_file = ref None
let arg_load_plugins = ref []
let arg_load_dirs = ref [ "." ]

(* secondary arguments *)
let arg_warnings = ref ( [] : string list )
let arg_errors = ref ([] : string list )
let arg_explicit_files = ref ([] : string list )

let arg_verbosity = Yalo.GState.verbosity
let arg_first_arg = ref ""
let arg_projects = ref ([] : string list)

let arg_print_config = ref false
let arg_skip_config_warnings = ref false
let arg_map_src_projects = ref true

let arg_save_config = ref (None : string option)
let arg_profiles = ref ([] : string list)

let arg_message_format = ref Yalo.Types.Format_Context
let arg_autofix_inplace = ref (None : bool option)
let arg_output = ref (None : string option)
let arg_summary = ref @@ Some 30

let parse_initial_args args =
  let rec iter args =
    match args with
      ("-C"|"config-file") :: file :: args ->
        begin
          match !arg_config_file with
          | None ->
              arg_config_file := Some file;
              iter args
          | Some file0 ->
              Printf.eprintf
                "Error in arguments: -C <config-file> cannot be used twice\n";
              Printf.eprintf " - first used with %s\n" file0;
              Printf.eprintf " - second used with %s\n" file;
              exit 2
        end;
    | ("-L"|"--load-plugin") :: file :: args ->
        arg_load_plugins := !arg_load_plugins @ [ file ];
        iter args
    | ("-P"|"--profile") :: file :: args ->
        arg_profiles := !arg_profiles @ [ file ];
        iter args
    | ("-I"|"--include-dir") :: dir :: args ->
        arg_load_dirs := dir :: !arg_load_dirs;
        iter args
    | ("-v"|"--verbose") :: args ->
        incr arg_verbosity;
        iter args
    | "-no-load-plugins" :: args ->
        arg_no_load_plugins := true;
        iter args
    | [] | [ "--help" ] ->
        arg_no_load_plugins := true;
        "help", []
    | cmd :: args -> cmd, args
  in
  let cmd, args = iter args in
  cmd, args

let initial_arg_too_late name =
  Printf.eprintf
    "Command Error: argument %s should be specified among initial arguments.\n"
    name;
  Printf.eprintf
    "  It should come before secondary arguments such as %s\n%!"
    !arg_first_arg;
  exit 2

let initial_arg_specs = [

  (* Initial arguments *)

  [ "L" ; "load-plugin" ], EZCMD.String (fun _s ->
      initial_arg_too_late "-L"),
  EZCMD.info
    ~docs:"INITIAL ARGUMENTS"
    ~docv:"PLUGIN"
    "Load plugin PLUGIN (a .cmxs or a .ml file)";

  [ "P" ; "profile" ], EZCMD.String (fun _s ->
      initial_arg_too_late "-P"),
  EZCMD.info
    ~docs:"INITIAL ARGUMENTS"
    ~docv:"PROFILE"
    "Specify profile to load (a yalo-<PROFILE>.conf file)";


  [ "I" ; "include-dir" ], EZCMD.String (fun _s ->
      initial_arg_too_late "-I"),
  EZCMD.info
    ~docs:"INITIAL ARGUMENTS"
    ~docv:"DIR"
    "Add DIR to the list of directories when plugins should be searched for";


  [ "C" ; "config-file" ], EZCMD.String (fun _s ->
      initial_arg_too_late "-C"),
  EZCMD.info
    ~docs:"INITIAL ARGUMENTS"
    ~docv:"CONFIG-FILE"
    "Load CONFIG-FILE instead of searching for .yalocaml";

  [ "no-load-plugins" ], EZCMD.Unit (fun () ->
      initial_arg_too_late "--no-load-plugins"),
  EZCMD.info
    ~docs:"INITIAL ARGUMENTS"
    "Do not load plugins"
]

let common_arg_specs = [

  [ "print-config" ], EZCMD.Set arg_print_config,
  EZCMD.info "Print configuration";

  [ "save-config" ], EZCMD.String (fun file ->
      arg_save_config := Some file),
  EZCMD.info ~docv:"FILE" "Save configuration to FILE";

]

