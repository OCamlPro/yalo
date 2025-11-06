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
   https://github.com/Kakadu/zanuda/blob/master/src/untyped/Dollar.ml
   for comparison
 *)

open Yalo.V1
open Yalo_plugin_ocaml.V1

open OCAML_AST

let lint_id = "camel_extra_dollar"
let lint_msg = "Useless `@@` operator"
let documentation =
  {|
### What it does
The `@@` operator is used for writing less parentheses in expression.
Code like `f (g (h x))` could be rewritten as `f @@ g (h x)`.
But is some cases it is not required, like `print_int @@ 1`.
Some of these cases are reported by this lint.

  |}
  |> Stdlib.String.trim

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

      let expression ~file ~linter pexp =
        match pexp.pexp_desc with
        | Pexp_apply
          ( { pexp_desc = Pexp_ident { txt = Lident "@@" ; _ }; _ }
          , [ _; (Nolabel, { pexp_desc = arg_exp ; _ }) ] )
          -> begin
            match arg_exp with
            | Pexp_ident _
              (* ... @@ 42 *)
              | Pexp_constant _
              (* ... @@ (1,2,...) *)
              | Pexp_tuple _
              (* ... @@ None *)
              | Pexp_construct (_, None)
              (* ... @@ { ... } *)
              | Pexp_record _  ->
               let loc = pexp.pexp_loc in
               YALO.warn ~file ~linter ~loc w
            | _ -> ()
          end
        | _ -> ()
      in
      traverse.expression <- (linter, expression) :: traverse.expression
    )
