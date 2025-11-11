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

open OCAML_AST
open Astutils (* for =| *)
open OCAML_CHECK

module type STRUCTURE_ITEM_CHECK = sig
  val lint_id : string
  val fix : string
  val violation : string
  val structure_item :
    warner ->
    OCAML_AST.structure_item ->
    unit
end

module MakeStructureItemCheck (M: STRUCTURE_ITEM_CHECK) =
  MAKE_STRUCTURE_ITEM(struct
    include M
    let lint_msg = M.violation
    let lint_doc =
      Printf.sprintf
        {|### Violation
           %s

           ### Correction
           %s
           |} M.violation M.fix

  end)

module USE_MAP =
  MakeStructureItemCheck(struct

    let lint_id = "use_map"

    let fix = "using a higher order function like transform"

    let violation = "overly verbose function implementation"

    let structure_item (warn : warner) pstr =
      match pstr.pstr_desc with
      | Pstr_value (OCAML_AST.Recursive, [vb]) ->
          let func_name = Astutils.ident_of_let vb in
          let func_body_exp = Astutils.body_of_fun vb.pvb_expr in
          begin match func_body_exp.pexp_desc with
            | Pexp_match (_, [c_empty; c_cons]) ->
                let empty_case_ok =
                  Astutils.is_pat_constr c_empty.pc_lhs "[]" &&
                  c_empty.pc_rhs =| "[]" in
                let tail_binding = Astutils.binding_of_lcase c_cons in
                let uses_func_ok =
                  Astutils.uses_func_recursively_list c_cons func_name
                    tail_binding in
                if empty_case_ok && uses_func_ok then
                  warn ()
            | _ -> ()
          end
      | _ -> ()


  end)


(** -------------- Checks rules: If top-level let should use
    List.fold_right ------------- *)

module USE_FOLD =
  MakeStructureItemCheck(struct

    let lint_id = "use_fold"
    let fix = "using a higher order function like fold"

    let violation = "overly verbose function implementation"

    let structure_item (warn : warner) pstr =
      match pstr.pstr_desc with
      | Pstr_value (OCAML_AST.Recursive, [vb]) ->
          let func_name = Astutils.ident_of_let vb in
          let func_body_exp = Astutils.body_of_fun vb.pvb_expr in
          begin match func_body_exp.pexp_desc with
            | Pexp_match (_, [c_empty; c_cons]) ->
                let empty_case_ok = Astutils.is_pat_constr
                    c_empty.pc_lhs "[]" &&
                                    not (c_empty.pc_rhs =| "()") in
                let tail_binding = Astutils.binding_of_lcase c_cons in
                let uses_func_ok = Astutils.uses_func_recursively_list_any
                    c_cons func_name tail_binding in
                if empty_case_ok && uses_func_ok then
                  warn ()
            | _ -> ()
          end
      | _ -> ()
  end)

(** -------------- Checks rules: If top-level let should use List.iter
    ------------------- *)

module USE_ITER =
  MakeStructureItemCheck(struct

    let lint_id = "use_iter"
    let fix = "using a higher order function like iter"

    let violation = "overly verbose function implementation"

    let structure_item (warn: warner) pstr =
      match pstr.pstr_desc with
      | Pstr_value (OCAML_AST.Recursive, [vb]) ->
          let func_name = Astutils.ident_of_let vb in
          let func_body_exp = Astutils.body_of_fun vb.pvb_expr in
          begin match func_body_exp.pexp_desc with
            | Pexp_match (_, [c_empty; c_cons]) ->
                let empty_case_ok = Astutils.is_pat_constr
                    c_empty.pc_lhs "[]" &&
                                    c_empty.pc_rhs =| "()" in
                let tail_binding = Astutils.binding_of_lcase c_cons in
                let uses_func_ok = Astutils.uses_func_recursively_seq c_cons
                    func_name tail_binding in
                if empty_case_ok && uses_func_ok then
                  warn ()
            | _ -> ()
          end
      | _ -> ()

  end)
