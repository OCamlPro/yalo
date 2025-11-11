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

module MAIN =
  EZCMD.MAKE(struct

    let command = "yalo"
    let about = {|yalo [INITIAL_ARGS] [SUBCOMMAND] [SUBCOMMAND_ARGUMENTS]|}
    let set_verbosity n = Args.arg_verbosity := n
    let get_verbosity () = !Args.arg_verbosity

    let backtrace_var = Some "YALO_BACKTRACE"
    let usage =
      {|
         Plugin-based project linter
         |}
    let version = Version.version

    exception Error of string

  end)

type command_kind =
  | LOAD_PLUGINS
  | NO_PLUGINS

let commands = [
  "lint", LOAD_PLUGINS, Command_lint.cmd ;
  "lint-no-plugins", NO_PLUGINS, Command_lint.cmd ;
  "doc", LOAD_PLUGINS, Command_doc.cmd ;
]


let main () =
  (* This is useless: Ezcmd.V2 only sets backtrace if the
     backtrace_var is defined or -v specified on command line *)
  Printexc.record_backtrace true;

  let args = Array.to_list Sys.argv in
  let cmd, args = match args with
      cmd :: args -> cmd, args
    | [] -> assert false
  in

  let subcmd, args = Args.parse_initial_args args in

  if String.length subcmd > 0 && subcmd.[0] = '-' then begin
    Printf.eprintf
      "Execution error: a subcommand is needed before argument %S\n%!" cmd;
    exit 2
  end;

  let argv = Array.of_list (cmd :: subcmd :: args) in

  let needs_to_load_plugins = ref false in
  List.iter (fun (name, kind, _sub) ->
      if subcmd = name then
        match kind with
        | LOAD_PLUGINS -> needs_to_load_plugins := true
        | NO_PLUGINS -> ()
    ) commands ;

  let _fs = Init.get_fs ~needs_to_load_plugins: !needs_to_load_plugins () in

  (* sub-commands arguments may come from plugins... *)
  let commands =
    List.map (fun (name, _kind, sub) -> sub name) commands
  in
  MAIN.main
    (* ~on_error:Cobol_common.keep_temporary_files *)
    (* ~on_exit:Cobol_common.exit *)
    (* ~print_config:Config.print *)
    ~common_args:[]
    ~argv
    commands
  ;

  begin match !Args.arg_save_config with
    | None -> ()
    | Some filename ->
        Yalo.Config.save filename;
        Printf.eprintf "Config saved to file %S\n%!" filename
  end;

  ()
