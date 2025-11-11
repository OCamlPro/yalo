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


open Yalo_plugin_ocaml.V1
open Astutils
open OCAML_AST
open OCAML_CHECK

(** ------------------ Checks rules: if (_ = [literals] | [literals] =
    _) ----------------------- *)
module EQ_LIST =
  Match.MakeExpressionCheck(struct
    let lint_id = "eq_list"
    let fix = "using a pattern match to check whether a list has a \
               certain value"
    let violation = "using `=` with lists as a condition in an if \
                     statement"
    let expression (warn : warner) e =
      match e.pexp_desc with
      | Pexp_apply (application, [(_,lop); (_,rop)]) ->
          if application =~ "=" &&
             (Astutils.is_list_lit lop || Astutils.is_list_lit rop)
          then
            warn ()
      | _ -> ()
  end)


(** ------------------ Checks rules: _ = [Some _] | [Some _] = _
    ------------- *)
module EQ_OPTION =
  Match.MakeExpressionCheck(struct
    let lint_id = "eq_option"

    let fix = "using a pattern match to check the presence of an option"
    let violation = "using `=` with options"
    let expression (warn: warner) e =
      match e.pexp_desc with
      | Pexp_apply (application, [(_, lop); (_, rop)]) ->
          if application =~ "=" && (Astutils.is_some_lit lop
                                    || Astutils.is_some_lit rop) then
            warn ()
      | _ -> ()
  end)

(** ------------------ Checks rules: (_ = :bool | :bool = _)
    ----------------------- *)
module EQ_BOOL =
  Match.MakeExpressionCheck(struct
    let lint_id = "eq_bool"

    let fix = "using the variable itself to represent the value"
    let violation = "using `=` with a boolean literal"
    let expression (warn: warner) e =
      match e.pexp_desc with
      | Pexp_apply (application, [(_,lop); (_,rop)]) ->
          if application =~ "=" && (Astutils.is_bool_lit lop ||
                                    Astutils.is_bool_lit rop) then
            warn ()
      | _ -> ()
  end)

(** ------------------ Checks rules: (_ == _)
    ----------------------- *)
module EQ_PHYSICAL =
  Match.MakeExpressionCheck(struct
    let lint_id = "eq_physical"

    let fix = "using `=` to evaluate structural equality"
    let violation = "using `==` when structural equality is intended"
    let expression (warn: warner) e =
      match e.pexp_desc with
      | Pexp_apply (application, [(_,_); (_,_)]) ->
          if application =~ "==" then warn ()
      | _ -> ()
  end)

(** ------------------ Checks rules: (_ != _)
    ----------------------- *)
module NEQ_PHYSICAL =
  Match.MakeExpressionCheck(struct
    let lint_id = "neq_physical"
    let fix = "using `<>` to evaluate structural inequality"
    let violation = "using `!=` when structural equality is intended"
    let expression (warn: warner) e =
      match e.pexp_desc with
      | Pexp_apply (application, [(_,_); (_,_)]) ->
          if application =~ "!=" then warn ()
      | _ -> ()
  end)

