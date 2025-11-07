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
   https://github.com/Kakadu/zanuda/blob/master/src/untyped/Casing.ml
   for comparison
 *)

let lint_id = "camel_cased_types"
let lint_msg = "Identifiers in camel case are discouraged"
let documentation =
  {|
### What it does
Checks that type names are using snake case (`very_useful_typ`) and not using
camel case (`veryUsefulTyp`) popular in Python and Haskell.

### Why is this bad?
Wrong casing is not exactly bad but OCaml tradition says that types' and
module types' names should be snake case.
Modules names' in standard library are in camel case but in most Janestreet
libraries (ppxlib, base) they are in snake case too.
  |}
  |> Stdlib.String.trim
let is_camel_case s = String.(lowercase_ascii s <> s)

let is_good_name s =
  EzString.starts_with s ~prefix:"_menhir_cell1_" || not (is_camel_case s)

open Yalo.V1
open Yalo_plugin_ocaml.V1

open OCAML_AST
;;

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
      let type_declaration ~file ~linter tdecl =
        let tname = tdecl.ptype_name.txt in
        let loc = tdecl.ptype_loc in
        if not (is_good_name tname) then
          YALO.warn ~file ~linter ~loc w
      in
      traverse.type_declaration <-
        (linter, type_declaration) :: traverse.type_declaration
    )
