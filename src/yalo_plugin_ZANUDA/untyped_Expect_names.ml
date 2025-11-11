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
   https://github.com/Kakadu/zanuda/blob/master/src/untyped/Expect_names.ml
   for comparison
*)

let lint_id = "expect_tests_no_names"
let lint_msg =
  {|A test without description. Try `let%expect_test "name" = ...`|}
let documentation =
  {|
### What it does
Warns about expect tests without descriptions: `let%expect_test _ = ...`

### Why?
For purposes of refactoring we want to know why a certain test was written.
It allows us to decide easily if this test is still needed.
Better version is `let%expect_test "decent name" = ...`
  |}
  |> Stdlib.String.trim

open Yalo.V1
open Yalo_plugin_ocaml.V1

open OCAML_AST

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
       let extension ~file ~linter (({ txt ; _ }, payload)) =
         if String.equal txt "expect_test"
         then begin
           match payload with
           | PStr
               [ { pstr_desc =
                     Pstr_value
                       (Nonrecursive,
                        [ { pvb_loc = loc;
                            pvb_pat = { ppat_desc = Ppat_any ;_ }; _ } ]);
                   _
                 }
               ] ->
               YALO.warn ~file ~linter ~loc w
           | _ -> ()
         end

       in
       traverse.extension <- (linter, extension) :: traverse.extension
    )
