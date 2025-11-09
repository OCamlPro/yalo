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
   https://github.com/Kakadu/zanuda/blob/master/src/typed/Printf.ml
   for comparison
 *)

(* This warning is disabled by default, because "%s" is not
   semantically equivalent to %S, because the later escapes the
   content of the string. *)

open Yalo.V1
open Yalo_plugin_ocaml.V1

open OCAML_AST

let lint_id = "format_module_usage"
let lint_msg =
  "The format string is too much verbose (rewrite \"%s\" -> %S)"
let documentation =
  {|
### What it does
OCaml formatted string are more powerful than C counterpart.
You should be aware of available features.

### Why is is important?
Shorter code is more readable. Rewrite

*  `\"%s\"` to `%S`
|}
  |> Stdlib.String.trim

let is_substring substring =
  let re = Re.str substring in
  let re = Re.compile re in
  Re.execp re

let check_quoted_pattern = is_substring {|"%s"|}

let register ~id ~tags
      ?(lint_id=lint_id) ?(msg=lint_msg) ?(desc=documentation)
      ?(set_by_default=false)
      ns =

  let w = YALO.new_warning ns id
            ~name:lint_id ~tags ~msg ~desc ~set_by_default in

  OCAML_LANG.new_tast_impl_traverse_linter
    ns ("check:" ^ lint_id)
    ~warnings:[w]
    OCAML_TAST.(fun ~file ~linter traverse ->
    let check_expr ~file:_ ~linter:_ expr =
      match expr.exp_desc with
      | Texp_construct (
          lid, _cstr_desc,
          [ _ ;
            { exp_loc = loc ;
              exp_desc = Texp_constant (Const_string _ as cst) ;
              _ }])
           when
             begin
               match Longident.flatten lid.txt with
               | ["CamlinternalFormatBasics" ; "Format"] -> true
               | _ -> false
             end
        ->
         if OCAML_TAST.extract_const_string cst |> fst |>
              check_quoted_pattern then
             YALO.warn ~loc ~file ~linter w
      | _ -> ()
    in
    traverse.expr <- (linter, check_expr) :: traverse.expr
  )
