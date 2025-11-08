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
open Ezcmd.V2
open Ez_file.V1
open Yalo_misc.Utils.OP

open Yalo.Types

let arg_output_dir = ref "."

let arg_specs = [

      [ "w" ; "warnings" ],
      EZCMD.String (fun s -> Args.arg_warnings := !Args.arg_warnings @ [s]),
      EZCMD.info ~docv:"SPEC"
        "Set warnings according to SPEC-ification";

      [ "e" ; "errors" ],
      EZCMD.String (fun s -> Args.arg_errors := !Args.arg_errors @ [s]),
      EZCMD.info ~docv:"SPEC"
        "Set errors according to SPEC-ification";

      [ "dir" ], EZCMD.String (fun s -> arg_output_dir := s),
      EZCMD.info ~docv:"DIRECTORY" "Target directory for output";
                ]



let clippy_gen dir =
  Yalo_misc.Utils.safe_mkdir dir ;

  let impls = ref StringSet.empty in
  let groups = ref StringSet.empty in
  let namespaces = ref StringSet.empty in
  let rules =
    List.map Yalo_misc.Clippy.(fun w ->
      let applicability = {
          is_multi_part_suggestion = false ;
          applicability = "Unresolved" ;
        } in
      let level = match w.w_level_warning,
                        w.w_level_error with
          _, 2 -> "deny"
        | 2, _ -> "warn"
        | _ -> "allow"
      in

      let namespace = w.w_namespace.ns_name in
      namespaces := StringSet.add namespace !namespaces ;

      let impl = w.w_namespace.ns_plugin.plugin_name in
      impls := StringSet.add impl !impls ;

      let tags = List.map (fun tag -> tag.tag_name) w.w_tags in
      let group = String.concat ":" tags in

      List.iter (fun tag ->
          groups := StringSet.add tag !groups) tags ;

      let docs = Printf.sprintf
                   "### Message\n%s\n### Description\n%s"
                   w.w_msg
                   w.w_desc
      in
      {
        id = Printf.sprintf "%s %s" w.w_idstr w.w_name;
        namespace ;
        group ;
        tags ;
        level ;
        impl ;
        docs ;
        applicability ;
      }
    ) (List.rev !Yalo.GState.all_warnings)
  in

  let json_file = dir // "lints.json" in
  EzFile.write_file json_file ( Yalo_misc.Clippy.json_of_rules rules );
  Printf.eprintf "File %s created\n%!" json_file;

  let js_content =
    Printf.sprintf
      {|
var NAMESPACES_FILTERS_DEFAULT = {
    "%s": true };
var IMPL_FILTERS_DEFAULT = {
    "%s": true };
var LEVEL_FILTERS_DEFAULT = {
    allow: true,
    warn: true,
    deny: true
};
var GROUPS_FILTER_DEFAULT = {
    "%s": true
};
|}
      (String.concat "\": true,\n    \"" (StringSet.to_list !namespaces))
      (String.concat "\": true,\n    \"" (StringSet.to_list !impls))
      (String.concat "\": true,\n    \"" (StringSet.to_list !groups))
  in

  let js_file = dir // "groups.js" in
  EzFile.write_file js_file js_content ;
  Printf.eprintf "File %s created\n%!" js_file;

  ()



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
    ~doc: "Generate the JSON documentation."
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
      let dir =
           Yalo_misc.Utils.normalize_filename ~subpath:fs.fs_subpath
             !arg_output_dir
      in
      clippy_gen dir
    )
