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
   https://github.com/Kakadu/zanuda/blob/master/
                                      src/typed/Var_should_not_be_used.ml
   for comparison *)

open Yalo.V1
open Yalo_plugin_ocaml.V1
open OCAML_AST

let lint_id = "var_should_not_be_used"
let lint_msg = "Identifier starting with '_' and used later"
let tags = [ "fpcourse" ; "readability" ]

let documentation =
  {|
### What it does
Report identifier starting with '_' and used later

### Why is this bad?
OCaml compiler has a tendency to report warning 26 about unused variables.
Usually this warning could be supressed by adding '_' in the beginning of
identifier to make it look like wildcard variable.
But if that identifier is used later it contradicts the purpose of adding
undescore in the beginnning.
  |}
  |> String.trim

let warning_msg id =
  Format.sprintf
    "Identifier `%s` used somewhere, but supposed to be unused."
    id

let register ~id ~tags
    ?(lint_id=lint_id) ?(msg=lint_msg) ?(desc=documentation)
    ns =

  let w = YALO.new_warning ns id
      ~name:lint_id ~tags ~msg ~desc
  in

  OCAML_LANG.new_tast_impl_traverse_linter
    ns ("check:" ^ lint_id)
    ~warnings:[w]
    OCAML_TAST.(fun ~file ~linter traverse ->

        let check_expr ~file:_ ~linter:_ exp =
          match exp.exp_desc with
          | Texp_ident (_, lid, _) ->
              let list = Longident.flatten lid.txt in
              List.iter (fun id ->
                  if id.[0] = '_' then
                    YALO.warn ~loc:exp.exp_loc
                      ~file ~linter w ~msg:(warning_msg id)
                ) list
          | _ -> ()
        in
        traverse.expr <- (linter, check_expr) :: traverse.expr
      )
