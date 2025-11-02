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

open Plugin

[%%if ocaml_version < (5, 4, 0)]
let translate_longident l = l
[%%else]
let translate_longident l = l.Asttypes.txt
[%%endif]

let main () =
  OCAML_LANG.new_tast_impl_traverse_linter ns "tast:check_longident"
    ~warnings:[ Untyped_linters.w_use_obj ]
    OCAML_TAST.(fun ~file ~linter traverse ->
    let rec check_longident ~loc l =
      match l with
        Longident.Lident s ->
         if s = "Obj" then
           YALO.warn loc ~file ~linter Untyped_linters.w_use_obj
        | Longident.Ldot (l, _) ->
           check_longident ~loc (translate_longident l)
        | Longident.Lapply (l1, l2) ->
           check_longident ~loc (translate_longident l1) ;
           check_longident ~loc (translate_longident l2) ;
    in
    let check_expr ~file:_ ~linter:_ expr =
      match expr.exp_desc with
      | Texp_ident (_path, { txt = l ; loc }, _) ->
         check_longident ~loc l
      | _ -> ()
    in
    traverse.expr <- (linter, check_expr) :: traverse.expr
  )
