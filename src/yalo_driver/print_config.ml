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

open EzCompat (* for IntMap *)
open Yalo.Types
open Yalo.Config.OP

let eprint () =

  Printf.eprintf "Current configuration (from config + args):\n%!";
  Printf.eprintf "   --no-load-plugins: %b\n%!" !Args.arg_no_load_plugins ;
  Printf.eprintf "   -C (--config-file): %s\n%!" (match !Args.arg_config_file with
    | None -> "none"
    | Some s -> Printf.sprintf "%S\n%!" s);
  Printf.eprintf "   -L (--load-plugin)\n%!";
  List.iter (fun plugin ->
      Printf.eprintf "        plugin %S\n%!" plugin
    ) !Args.arg_load_plugins ;
  Printf.eprintf "   -I (--include-dir)\n%!";
  List.iter (fun dir ->
      Printf.eprintf "        dir %S\n%!" dir
    ) !Args.arg_load_dirs ;
  Printf.eprintf "   -w (--warnings)\n%!";
  List.iter (fun dir ->
      Printf.eprintf "        warnings %S%s\n%!" dir
        (if !Args.arg_skip_config_warnings then
          " (skipped)" else "")
    ) !!Yalo.Config.config_warnings ;
  List.iter (fun dir ->
      Printf.eprintf "        warnings %S\n%!" dir
    ) !Args.arg_warnings ;
  Printf.eprintf "   -e (--errors)\n%!";
  List.iter (fun dir ->
      Printf.eprintf "        errors %S%s\n%!" dir
        (if !Args.arg_skip_config_warnings then
          " (skipped)" else "")
    ) !Args.arg_errors ;
  Printf.eprintf "   -e (--errors)\n%!";
  List.iter (fun dir ->
      Printf.eprintf "        errors %S\n%!" dir
    ) !Args.arg_errors ;
  Printf.eprintf "   --source-dir\n%!";
  List.iter (fun dir ->
      Printf.eprintf "        project %S\n%!" dir
    ) !Args.arg_projects ;
  Printf.eprintf "   --verbose: %d\n%!" !Args.arg_verbosity ;

  (*
  Printf.eprintf "   --lint-ast-from-cmt: %b\n%!" !Args.arg_lint_ast_from_cmt ;
  Printf.eprintf "   --no-lint-ast-from-src: %b\n%!" (not !Args.arg_lint_ast_from_src) ;
   *)
  
  Printf.eprintf "   --skip-config-warnings: %b\n%!" !Args.arg_skip_config_warnings ;

  (* TODO: show plugins and language too *)
  Printf.eprintf "Namespaces:\n%!";
  Hashtbl.iter (fun _ ns ->
      Printf.eprintf "  Namespace %S\n%!" ns.ns_name ;
      Printf.eprintf "  Warnings:\n%!";
      IntMap.iter (fun _ w ->
          Printf.eprintf "     %d%s %s [%s]\n%!"
            w.w_num
            (if w.w_level_error then "+e"
            else
              if w.w_level_warning then "+w"
              else "-")
            w.w_name
            (String.concat " "
               (List.map (fun t -> t.tag_name) w.w_tags))
        ) ns.ns_warnings ;
    ) Yalo.Engine.all_namespaces ;

  Printf.eprintf "Projects:\n%!";
  Hashtbl.iter (fun _ p ->
      Printf.eprintf "  Project %S\n%!" p.project_name ;
      List.iter (fun file ->
          Printf.eprintf "    %S\n%!" file.file_name
        )
        (
          p.project_files
        ) 
    ) Yalo.Engine.all_projects ;

  ()  
