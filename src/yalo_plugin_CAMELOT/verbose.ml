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
(* This code was taken from github.com/upenn-cis1xx/camelot        *)
(* that was distributed under the terms of the Apache-2.0 license. *)


open Yalo_plugin_ocaml.V1
open Astutils
open OCAML_AST
open OCAML_AST_CHECK

(** ----------------------- Checks rules: [_] \@ _
    ---------------------------- *)

module LIT_PREPEND =
  Match.MakeExpressionCheck(struct
      let lint_id = "lit_prepend"
      let fix = "using `::` instead"
      let violation = "using `@` to prepend an element to a list"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_apply (application, [(_, lop); _]) ->
           if application =~ "@" && Astutils.is_singleton_list lop then
             warn ()
        | _ -> ()

    end)

(** ----------------------- Checks rules: fst/snd t
    -------------------------- *)

module TUPLE_PROJ =
  Match.MakeExpressionCheck(struct
      let lint_id = "tuple_proj"

      let fix = "using a let pattern match statement instead"
      let violation = "using fst / snd to project values out of a tuple"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_apply (application, [_]) ->
           if application =~ "fst" || application =~ "snd" then
             warn ()
        | _ -> ()
    end)


(** ----------------------- Checks rules: Nesting if >= 3 levels
    ------------- *)

module NESTED_IF =
  Match.MakeExpressionCheck(struct
      let lint_id = "nested_if"

      let fix = "using let statements or helper methods / rethinking logic"
      let violation =
        "using nested if statements more than three layers deep"

      let expression (warn : warner) e =
        let rec find_nesting (p: OCAML_AST.expression_desc) depth=
          depth = 0 ||
            begin match p with
            | Pexp_ifthenelse (_, bthen, belse) ->
               depth = 1 ||
                 find_nesting
                   ((Astutils.skip_seq_let bthen).pexp_desc) (depth - 1) ||
                   (match belse with
                   | None -> false
                   | Some belse ->
                      find_nesting
                        ((Astutils.skip_seq_let belse).pexp_desc) (depth - 1))
            | _ -> false
            end
        in
        if find_nesting e.pexp_desc 4 then warn ()
    end)

(** -------------------- Checks rules: Nesting match >= 3 levels
    ------------- *)

module NESTED_MATCH =
  Match.MakeExpressionCheck(struct
      let lint_id = "nested_match"
      let fix = "using let statements or helper methods / rethinking logic"
      let violation = "using nested match statements three or more layers deep"
      let expression (warn : warner) e =
        match e.pexp_desc with
        (* Layer one *)
        | Pexp_match (_, cs) ->
           let matches_three =
             List.map (fun (c: OCAML_AST.case) ->
                 c.pc_rhs |> Astutils.skip_seq_let) cs |>
               (* Grab the second layer *)
               List.map (fun (e: OCAML_AST.expression) ->
                   match e.pexp_desc with
                   | Pexp_match (_, cs) ->
                      List.map (fun (c: OCAML_AST.case) ->
                          c.pc_rhs |> Astutils.skip_seq_let) cs
                   | _ -> []
                 ) |> List.flatten |>
               (* Grab the third layer *)
               List.exists (fun (e: OCAML_AST.expression) ->
                   match e.pexp_desc with
                   | Pexp_match _ -> true
                   | _ -> false ) in
           if matches_three then warn ()
        | _ -> ()
    end)

(** ------------ Checks rules: if _ then/else true | false
    ------------------ *)

module IF_RETURNS_LIT =
  Match.MakeExpressionCheck(struct
      let lint_id = "if_returns_lit"
      let fix = "returning just the condition (+ some tweaks)"
      let violation = "using an if statement to return `true | false` literally"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_ifthenelse (_, bthen, Some belse) ->
           if (bthen =| "true" || bthen =| "false") &&
                (belse =| "false" || belse =| "true") then
             warn ()
        | _ -> ()
    end)

(** ------------ Checks rules: if cond then cond | if cond then _ else
                      cond -- *)

module IF_COND_THEN_COND =
  Match.MakeExpressionCheck(struct
      let lint_id = "if_cond_then_cond"
      let fix = "returning just the condition or simplifying further"
      let violation = "returning the condition of an if statement on \
                       success and a boolean literal otherwise"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_ifthenelse (cond, bthen, Some belse) ->
           if (belse =| "false" || belse =| "true")
                && Astutils.e_eq cond bthen
           then
             warn ()
           else
             if (bthen =| "false" || bthen =| "true")
                && Astutils.e_eq cond belse
             then
               warn ()
        | _ -> ()
    end)

(** ------------ Checks rules: if not cond then x else y
    --------------------- *)

module IF_NOT_COND =
  Match.MakeExpressionCheck(struct
      let lint_id = "if_not_cond"
      let fix = "swapping the then and else branches of the if statement"
      let violation = "checking negation in the if condition"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_ifthenelse (cond, _, _) ->
           begin match cond.pexp_desc with
           | Pexp_apply (func, [_]) ->
              if func =~ "not" then
                warn ()
           | _ -> ()
           end
        | _ -> ()
    end)

(** ------------ Checks rules: if x then true else y
    ------------------------- *)

module IF_TO_OR =
  Match.MakeExpressionCheck(struct
      let lint_id = "if_to_or"
      let fix = "rewriting using a boolean operator like `||`"
      let violation = "overly verbose if statement that can be simplified"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_ifthenelse (_cond, bthen, Some belse) ->
           if not (belse =| "true") &&
                not (belse =| "false") &&
                  bthen =| "true" then
             warn ()
        | _ -> ()
    end)

(** ------------ Checks rules: if x then y else false
    ------------------------ *)

module IF_TO_AND =
  Match.MakeExpressionCheck(struct
      let lint_id = "if_to_and"
      let fix = "rewriting using a boolean operator like `&&`"
      let violation = "overly verbose if statement that can be simplified"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_ifthenelse (cond, bthen, Some belse) ->
           if not (bthen =| "true") &&
                not (bthen =| "false") &&
                  belse =| "false" &&
                    Astutils.e_neq cond bthen
           then
             warn ()
        | _ -> ()
    end)

(** ------------ Checks rules: if x then false else y
    ------------------------ *)

module IF_TO_AND_INV =
  Match.MakeExpressionCheck(struct
      let lint_id = "if_to_and_inv"
      let fix = "rewriting using a boolean operator like `&&` and `not`"
      let violation = "overly verbose if statement that can be simplified"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_ifthenelse (_cond, bthen, Some belse) ->
           if not (belse =| "true") && not (belse =| "false")
              && bthen =| "false" then
             warn ()
        | _ -> ()
    end)

(** ------------ Checks rules: if x then y else true
    ------------------------ *)

module IF_TO_OR_INV =
  Match.MakeExpressionCheck(struct
      let lint_id = "if_to_or_inv"
      let fix = "rewriting using a boolean operator like `||` and `not`"
      let violation = "overly verbose if statement that can be simplified"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_ifthenelse (_cond, bthen, Some belse) ->
           if not (bthen =| "true") && not (bthen =| "false") &&
                belse =| "true" then
             warn ()
        | _ -> ()
    end)

(** ------------ Checks rules: ... || true | true || ... | false ||
    ... | ... || false -- *)

module REDUNDANT_OR =
  Match.MakeExpressionCheck(struct
      let lint_id = "redundant_or"
      let fix = "simplifying further"
      let violation = "Usage of the `||` is redundant"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_apply (appl, [(_, l);(_, r)]) ->
           if appl =~ "||" &&
                (Astutils.e_eq_any
                   (Astutils.smash_boolean_tree e.pexp_desc) ||
                   l =| "true" ||
                     l =| "false" ||
                       r =| "true" ||
                         r =| "false") then
             warn ()
        | _ -> ()
    end)

(** ------------ Checks rules: ... && true | true && ... | false &&
    ... | ... && false -- *)

module REDUNDANT_AND =
  Match.MakeExpressionCheck(struct
      let lint_id = "redundant_and"
      let fix = "simplifying further"
      let violation = "Usage of the `&&` is redundant"
      let expression (warn : warner) e =
        match e.pexp_desc with
        | Pexp_apply (appl, [(_, l);(_, r)]) ->
           if appl =~ "&&" &&
                (Astutils.e_eq_any
                   (Astutils.smash_boolean_tree e.pexp_desc) ||
                   l =| "true" ||
                     l =| "false" ||
                       r =| "true" ||
                         r =| "false") then
             warn ()
        | _ -> ()
    end)

