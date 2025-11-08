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

open Ppxlib

let raise_on_lint_error = ref false

let () =
  Driver.add_arg
    "-raise-on-lint-error"
    (Set raise_on_lint_error)
    ~doc:
    " Report an error during linting rather than injecting an error \
     node. This is particularly useful when using the [lint] dune \
     stanza, which ignores typical lint errors."

open Yalo.V1
open Yalo_plugin_ocaml.V1

let lint_error msg =
  Ppxlib.Driver.Lint_error.of_string
    { msg.Yalo.Types.msg_loc
      with loc_ghost = true } msg.msg_string

(* TODO: find the name of the file ! *)
let new_dummy_file file_kind =
  let fs = Yalo.Main.init () in
  (* TODO : we should probably create a real tree of folders to the
     document instead of a faked name, in case the name is used for
     linting, so that we can benefit from the fileattrs *)
  let basename = "<ppxlib>" in
  let file_doc = YALO_INTERNAL.get_document fs.fs_folder basename in
  YALO_INTERNAL.new_file basename ~file_doc ~file_kind
    ~file_crc:(Digest.string "")

let () =
  Driver.register_transformation
    "ppx_yalo"
    ~lint_intf:(fun sg ->
      let file = new_dummy_file OCAML_LANG.mli_file in
      Yalo.Lint_project.activate_warnings_and_linters ([],[]);
      Yalo_plugin_ocaml.Main.lint_ast_intf ~file sg ;
      let messages = Yalo.Engine.get_messages () in
      List.map lint_error messages)
    ~lint_impl:(fun st ->
      let file = new_dummy_file OCAML_LANG.ml_file in
      Yalo.Lint_project.activate_warnings_and_linters ([],[]);
      Yalo_plugin_ocaml.Main.lint_ast_impl ~file st ;
      let messages = Yalo.Engine.get_messages () in
      List.map lint_error messages)
    ~intf:(fun sg ->
      let file = new_dummy_file OCAML_LANG.mli_file in
      Yalo.Lint_project.activate_warnings_and_linters ([],[]);
      Yalo_plugin_ocaml.Main.lint_ast_intf ~file sg ;
      let messages = Yalo.Engine.get_messages () in
      Yalo.Message_format.display_messages messages ;
      sg)
    ~impl:(fun st ->
      let file = new_dummy_file OCAML_LANG.ml_file in
      Yalo.Lint_project.activate_warnings_and_linters ([],[]);
      Yalo_plugin_ocaml.Main.lint_ast_impl ~file st ;
      let messages = Yalo.Engine.get_messages () in
      Yalo.Message_format.display_messages messages ;
      st)
