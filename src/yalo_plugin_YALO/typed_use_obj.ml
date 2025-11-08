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

let register ns w =
  OCAML_LANG.new_tast_impl_traverse_linter ns
    ("check:typed:" ^ YALO_WARNING.name w)
    ~warnings:[ w ]
    OCAML_TAST.(fun ~file ~linter traverse ->
    let check_expr ~file:_ ~linter:_ expr =
      match expr.exp_desc with
      | Texp_ident (path, _, _) ->
         begin
           let path = Path.name path in
           if EzString.starts_with path ~prefix:"Stdlib.Obj" then
             let loc = expr.exp_loc in
             YALO.warn ~loc ~file ~linter w
         end
      | _ -> ()
    in
    traverse.expr <- (linter, check_expr) :: traverse.expr
  )
