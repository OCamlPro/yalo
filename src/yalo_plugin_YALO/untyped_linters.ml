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
open Plugin

let w_use_obj =
  YALO.new_warning ns
    ~name:"use-obj" 7
    ~tags:[ tag_untyped ]
    ~msg:"The use of the Obj module is dangerous"

let () =
  OCAML_LANG.new_ast_impl_traverse_linter ns "ast:check_longident"
    ~warnings:[ w_use_obj ]
    (fun ~file:_ ~linter traverse ->
      let check_longident ~file ~linter (loc, l) =
        let rec check_longident l =
          match l with
            Lident s ->
             if s = "Obj" then
               YALO.warn loc ~file ~linter w_use_obj
            | Ldot (l, _) ->
               check_longident l
            | Lapply (l1, l2) ->
               check_longident l1 ;
               check_longident l2 ;
        in
        check_longident l
      in
      traverse.longident <- (linter, check_longident) :: traverse.longident
    )

let w_use_external =
  YALO.new_warning ns
    ~name:"use-external" 8
    ~tags:[ tag_untyped ]
    ~msg:"The use of the external is dangerous"

let main () =
  OCAML_LANG.new_ast_impl_traverse_linter ns "ast:check_structure"
    ~warnings:[ w_use_external ]
    (fun ~file:_ ~linter traverse ->
      let str_item ~file ~linter str =
        match str.pstr_desc with
        | Pstr_primitive { pval_prim ; _ } when pval_prim <> [] ->
           let loc = str.pstr_loc in
           YALO.warn loc ~file ~linter w_use_external
        | _ -> ()
      in
      traverse.structure_item <-
        (linter, str_item) :: traverse.structure_item ;
    )
