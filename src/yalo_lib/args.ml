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
let arg_source_directories = ref ([] : string list )
let arg_build_directories = ref ([] : string list )

let arg_lint_ast_from_cmt = ref false
let arg_lint_ast_from_src = ref true
let arg_verbose = ref 0
let arg_first_arg = ref ""
let arg_projects = ref ([] : string list)

let arg_print_config = ref false
let arg_skip_config_warnings = ref false
let arg_map_src_projects = ref true

let arg_save_config = ref None
let arg_profile = ref None

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
            Printf.eprintf "Error in arguments: -C <config-file> cannot be used twice\n";
            Printf.eprintf " - first used with %s\n" file0;
            Printf.eprintf " - second used with %s\n" file;
            exit 2
       end;
    | ("-L"|"--load-plugin") :: file :: args ->
       arg_load_plugins := !arg_load_plugins @ [ file ];
       iter args
    | ("-I"|"--include-dir") :: dir :: args ->
       arg_load_dirs := dir :: !arg_load_dirs;
       iter args
    | ("-v"|"--verbose") :: args ->
       incr arg_verbose;
       iter args
    | [] -> arg_no_load_plugins := true; []
    | "-no-load-plugins" :: args ->
       arg_no_load_plugins := true;
       iter args
    | args -> args
  in
  let args = iter args in
  let args = match args with
    | [] -> [ "--help" ]
    | first_arg :: _ ->
       arg_first_arg := first_arg;
       args in
  args

let initial_arg_too_late name =
  Printf.eprintf
    "Command Error: argument %s should be specified among initial arguments.\n" name;
  Printf.eprintf "  It should come before secondary arguments such as %s\n%!" !arg_first_arg;
  exit 2


let args = [
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

    ["lint-ast-from-cmt"], EZCMD.Set arg_lint_ast_from_cmt,
    EZCMD.info "Call parsetree linters on cmt files";

    ["no-lint-ast-from-src"], EZCMD.Clear arg_lint_ast_from_src,
    EZCMD.info "Don't parse and call parsetree linters on source files";

    [ "print-config" ], EZCMD.Set arg_print_config,
    EZCMD.info "Print configuration";

    [ "save-config" ], EZCMD.String (fun file ->
                           arg_save_config := Some file),
    EZCMD.info ~docv:"FILE" "Save configuration to FILE";

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

    (* Initial arguments *)

    [ "L" ; "load-plugin" ], EZCMD.String (fun _s ->
                 initial_arg_too_late "-L"),
    EZCMD.info
      ~docs:"INITIAL ARGUMENTS"
      ~docv:"PLUGIN"
      "Load plugin PLUGIN (a .cmxs or a .ml file)";


    [ "I" ; "include-dir" ], EZCMD.String (fun _s ->
                 initial_arg_too_late "-I"),
    EZCMD.info
      ~docs:"INITIAL ARGUMENTS"
      ~docv:"DIR"
      "Add DIR to the list of directories when plugins should be searched for";


    [ "v" ; "verbose" ], EZCMD.Unit (fun () ->
                 initial_arg_too_late "-I"),
    EZCMD.info
      ~docs:"INITIAL ARGUMENTS"
      "Increase verbosity";


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

let cmd = EZCMD.sub
            "yalo"
            ~doc:"Plugin-based linter for OCaml files"
            ~args
            ~man:[]
            (fun () -> ())


let parse args =
  
  EZCMD.main cmd ~argv:(Array.of_list (Sys.argv.(0) :: args));

  begin
    match !arg_explicit_files,
          !arg_source_directories,
          !arg_build_directories with
    | [], [], [] ->
       arg_source_directories := [ "." ];
       arg_build_directories := [ "_build/default" ];
    | [ "." ], _, _ ->
       arg_explicit_files := [] ;
       arg_source_directories := "." :: !arg_source_directories;
       arg_build_directories := "_build/default" :: !arg_build_directories
    | _ -> ()
  end;
  ()

