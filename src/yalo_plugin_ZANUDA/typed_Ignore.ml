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
   https://github.com/Kakadu/zanuda/blob/master/src/typed/Ignore.ml
   for comparison
 *)

open Yalo.V1
open Yalo_plugin_ocaml.V1

let lint_id = "wrong_ignoring"
let lint_msg =  "Unsafe ignore. It's recommended to rewrite it with a let"
let documentation =
  {|
### What it does
Using 'Stdlib.ignore' is discouraged. It's better to rewrite it with let.

#### Explanation

Let's look at expression 'ignore (f x)'. If in the future the function 'f'
will accept one more argument or change return type the code above may
become buggy, because the function will not be fully applied and executed
(although the warning may be raised here if this warning is not masked).
It's recommended to rewrite the code as 'let (_ : int) = f x in ...`
where 'int' is an example of return type of the function 'f'.
|}
  |> Stdlib.String.trim

let warning_msg typed_exp =
  let untyped_exp = OCAML_TAST.untype_expression typed_exp in
  let si =
    Format.asprintf
      "let (_: %a) = %a"
      Printtyp.type_expr
      typed_exp.exp_type
      Pprintast.expression
      untyped_exp
  in
  Format.sprintf "Unsafe ignore. It's recommended to rewrite it as '%s'%!" si


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
      | Texp_apply (
          { exp_desc = Texp_ident (path, _, _); _ },
          [ _, arg ] )
           when (
        match Path.name path with
        | "Stdlib.ignore"
          | "Base.ignore"-> true
        | _ -> false ) ->
         begin
           match extract_arg arg with
           | None -> assert false
           | Some arg ->
              YALO.warn ~loc:exp.exp_loc
                ~file ~linter w ~msg:(warning_msg arg)
         end
      | _ -> ()
    in
    traverse.expr <- (linter, check_expr) :: traverse.expr
  )
