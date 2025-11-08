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

open Yalo.V1
open Yalo_plugin_ocaml.V1

open OCAML_AST

let lint_msg = "The use of external values is dangerous"

let register ns
      ?(name="use_external")
      ~tags
      ?(msg = lint_msg)
      id
  =
  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~msg
  in

  OCAML_LANG.new_ast_impl_traverse_linter ns
    ("check:" ^ name)
    ~warnings:[ w ]

    (fun ~file:_ ~linter traverse ->
      let str_item ~file ~linter str =
        match str.pstr_desc with
        | Pstr_primitive { pval_prim ; _ } when pval_prim <> [] ->
           let loc = str.pstr_loc in
           YALO.warn ~loc ~file ~linter w
        | _ -> ()
      in
      traverse.structure_item <-
        (linter, str_item) :: traverse.structure_item ;
    )
