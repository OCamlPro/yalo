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

(** Limited notion of expression equality - trees should look the same
    for commonly used expressions eee
*)
let rec exp_eq (el: OCAML_AST.expression) (er: OCAML_AST.expression) =
  exp_desc_eq el.pexp_desc er.pexp_desc

and exp_desc_eq (el: OCAML_AST.expression_desc)
(er: OCAML_AST.expression_desc) =
  match el, er with
  | Pexp_ident {txt = Lident i; _}, Pexp_ident {txt = Lident j; _} -> i = j
  | Pexp_constant c, Pexp_constant d -> const_eq c d
  | Pexp_let (lrec, _lvblist, el'), Pexp_let (rrec, _rvblist, er') ->
    (* Ignore the value bindings ooo *)
    lrec = rrec && exp_eq el' er'
  | Pexp_apply (el,largs), Pexp_apply (er, rargs) ->
    exp_eq el er &&
    List.for_all2 (fun (_, l) (_, r) -> exp_eq l r ) largs rargs
  | Pexp_tuple ls, Pexp_tuple rs -> List.for_all2 (exp_eq) ls rs
  | Pexp_construct ({txt = Lident l; _}, None),
    Pexp_construct ({txt = Lident r; _}, None) ->
    l = r
  | Pexp_construct ({txt = Lident l; _}, Some el),
    Pexp_construct ({txt = Lident r; _}, Some er) ->
    l = r && exp_eq el er
  | _ -> false

and value_binding_eq (el: OCAML_AST.value_binding)
(er: OCAML_AST.value_binding) =
  exp_eq el.pvb_expr er.pvb_expr &&
  pat_eq el.pvb_pat er.pvb_pat

and pat_eq (_el: OCAML_AST.pattern) (_er: OCAML_AST.pattern) =
  false

and const_eq (el: OCAML_AST.constant) (er: OCAML_AST.constant) = el = er
