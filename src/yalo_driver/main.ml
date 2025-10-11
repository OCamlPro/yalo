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

(* open EzCompat *)
(* open Ez_file.V1 *)
(* open Yalo.Config.OP *)

let common_inited = ref false
let common_init () =
  if not !common_inited then begin
      common_inited := true ;
      Yalo.Main.init
        ?config_file: !Args.arg_config_file
        ~load_dirs: !Args.arg_load_dirs
        ~plugins: !Args.arg_load_plugins
        ~no_load_plugins: !Args.arg_no_load_plugins
        ();
    end

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
  ]


let main () =

  let args = Array.to_list Sys.argv in
  let cmd, args = match args with
      cmd :: args -> cmd, args
    | [] -> assert false
  in

  let subcmd, args = Args.parse_initial_args args in

  let argv = Array.of_list (cmd :: subcmd :: args) in

  let _v = NO_PLUGINS in
  List.iter (fun (name, kind, _sub) ->
      if subcmd = name then
        match kind with
        | LOAD_PLUGINS -> common_init ()
        | NO_PLUGINS -> ()
    ) commands ;

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
  ()
