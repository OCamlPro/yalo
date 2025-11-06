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
(** Copyright 2021-2025, Kakadu. *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)
(* see
   https://github.com/Kakadu/zanuda/blob/master/src/untyped/Toplevel_eval.ml
   for comparison
 *)

open Yalo.V1
open Yalo_plugin_ocaml.V1

open OCAML_AST

let lint_id = "no_toplevel_eval"
let lint_msg =
  "Toplevel evaluations are not recommended"

let documentation =
  {|
### What it does
Adding toplevel evaluation statements is not recommended because it forces to
add `;;`. Rewrite using `let () = ...`
  |}
  |> Stdlib.String.trim

let is_doc_attribute attr = String.equal "ocaml.doc" attr.attr_name.txt

let register
      ~id
      ~tags
      ?(lint_id=lint_id)
      ?(msg=lint_msg)
      ?(desc=documentation)
      ns =

  let w = YALO.new_warning ns id
            ~name:lint_id ~tags ~msg ~desc
  in

  OCAML_LANG.new_ast_impl_traverse_linter
    ns ("check:" ^ lint_id)
    ~warnings:[w]
    (fun ~file:_ ~linter traverse ->

      let structure_item ~file ~linter pstr =
        match pstr.pstr_desc with
        | Pstr_eval (_, _) ->
           let stack = OCAML_AST_TRAVERSE.node_stack () in
           (* OCAML_AST_TRAVERSE.print_node_stack (); *)
           begin
             match stack with
             | OCAML_AST_TRAVERSE.Node_payload _ :: _ -> ()
             | _ ->
                let loc = pstr.pstr_loc in
                YALO.warn ~loc ~file ~linter w
           end
        | _ -> ()
      in
      traverse.structure_item <-
        (linter, structure_item) :: traverse.structure_item
    )
