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

let lint_msg = "The use of the Obj module is dangerous"

let register ns
    ?(name="use_obj")
    ~tags
    ?(msg = lint_msg)
    id
  =
  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~msg
  in
  if OCAML_LANG.is_running_as_ppx () then
    OCAML_LANG.new_ast_impl_traverse_linter ns
      ("check:" ^ name)
      ~warnings:[ w ]
      (fun ~file:_ ~linter traverse ->
         let check_longident ~file ~linter (loc, l) =
           let not_menhir_generated =
             not @@ OCAML_LANG.is_menhir_generated_file() in
           if not_menhir_generated then
             let path = OCAML_AST.longident_name l in
             if EzString.starts_with path ~prefix:"Stdlib.Obj" ||
                EzString.starts_with path ~prefix:"Obj"
             then
               YALO.warn ~loc ~file ~linter w
         in
         traverse.longident <- (linter, check_longident) :: traverse.longident
      )
  else
    OCAML_LANG.new_tast_impl_traverse_linter ns
      ("check:typed:" ^ YALO_WARNING.name w)
      ~warnings:[ w ]
      OCAML_TAST.(fun ~file ~linter traverse ->
          let not_menhir_generated =
            not @@ OCAML_LANG.is_menhir_generated_file() in
          let check_expr ~file:_ ~linter:_ expr =
            match expr.exp_desc with
            | Texp_ident (path, _, _) when not_menhir_generated ->
                let path = Path.name path in
                if EzString.starts_with path ~prefix:"Stdlib.Obj" ||
                   EzString.starts_with path ~prefix:"Obj"
                then
                  let loc = expr.exp_loc in
                  YALO.warn ~loc ~file ~linter w
            | _ -> ()
          in
          traverse.expression <- (linter, check_expr) :: traverse.expression
        )
