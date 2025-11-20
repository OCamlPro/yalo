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

open EzCompat
open Yalo.V1
open Yalo_plugin_ocaml.V1
open YALO_INFIX

let lint_msg = "If two arguments have the same type, they should be labeled"

open OCAML_TAST

[%%if ocaml_version < (5,2,0)]
let check e =
  match e.exp_desc with
  | Texp_function {
      cases = [
        { c_rhs = {
              exp_desc = Texp_function _ ;
              exp_loc ; _ ;
            }; _ };
      ];
      _ }
    when e.exp_loc.loc_ghost && not exp_loc.loc_ghost
    -> true
  | _ -> false
[%%else]
let check e =
  match e.exp_desc with
  | Texp_function (_ :: _,
                   Tfunction_body
                     { exp_desc = Texp_function _ ;
                       exp_loc ; _ ;
                     })
    when e.exp_loc.loc_ghost && not exp_loc.loc_ghost
    -> true
  | _ -> false
[%%endif]

[%%if ocaml_version < (4,11,0)]
let pat_defines_ident pat =
  match pat.pat_desc with
  | Tpat_var (ident, str)  -> Some (ident,str)
  | _ -> None
[%%elif ocaml_version < (5,2,0)]
let pat_defines_ident :
  type k. k general_pattern -> ( Ident.t * string Location.loc) option =
  fun pat ->
  match pat.pat_desc with
  | Tpat_var (ident, str)  -> Some (ident,str)
  | _ -> None
[%%else]
let pat_defines_ident :
  type k. k general_pattern -> ( Ident.t * string Location.loc) option =
  fun pat ->
  match pat.pat_desc with
  | Tpat_var (ident, str,_uid)  -> Some (ident,str)
  | _ -> None
[%%endif]

[%%if ocaml_version < (4,13,0)]
let types_get_desc t = (Btype.repr t).desc
[%%else]
let types_get_desc = Types.get_desc
[%%endif]

[%%if ocaml_version < (4,13,0)]
let printtyp_prepare_for_printing list =
  List.iter Printtyp.mark_loops list
let printtyp_prepared_type_expr = Printtyp.type_expr
[%%elif ocaml_version < (5,3,0)]
let printtyp_prepare_for_printing = Printtyp.prepare_for_printing
let printtyp_prepared_type_expr = Printtyp.prepared_type_expr
[%%else]
let printtyp_prepare_for_printing _list = ()
let printtyp_prepared_type_expr = Printtyp.type_expr
[%%endif]


let check_pat ty env min_args_to_need_labels =
  let rec iter list env ty =
    match types_get_desc (Ctype.expand_head env ty) with
    | Tarrow (Nolabel, ty_arg, ty_res, _ ) ->
        iter (ty_arg :: list) env ty_res
    | Tarrow (_, _ty_arg, ty_res, _ ) ->
        iter list env ty_res
    | _ ->
        if List.compare_length_with list !!min_args_to_need_labels < 0 then
          None
        else begin
          printtyp_prepare_for_printing list ;
          let rec check set list =
            match list with
            | []  -> None
            | ty :: list ->
                let ty =
                  OCAML_AST.format_to_string
                    printtyp_prepared_type_expr ty
                in
                if StringSet.mem ty set then
                  Some ty
                else
                  check (StringSet.add ty set) list
          in
          check StringSet.empty list
        end
  in
  iter [] env ty

let register ns
    ?(name="need_labels")
    ~tags
    ?(msg = lint_msg)
    section
    id
  =
  let ns_name = YALO_NS.name ns in

  let min_args_to_need_labels =
    YALO.CONFIG.create_option section
      ~path:[ ns_name; "min_args_to_need_labels"]
      ~short_help:"Minimal number of arguments for a function to need \
                   to discriminate between arguments of same types"
      EzConfig.int_option
      3
  in

  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~msg
  in
  OCAML_LANG.new_tast_impl_traverse_linter ns
    ("check:typed:" ^ YALO_WARNING.name w)
    ~warnings:[ w ]
    OCAML_TAST.(fun ~file ~linter traverse ->
        let version_pat ~file:_ ~linter:_ pat =
          match pat_defines_ident pat with
          | None -> ()
          | Some _ ->
              match check_pat pat.pat_type pat.pat_env
                      min_args_to_need_labels with
              | Some ty ->
                  let msg =
                    Printf.sprintf "This function of type \"%s\" should \
                                    use labels to discriminate args of \
                                    type \"%s\""
                      (OCAML_AST.format_to_string
                         Printtyp.type_expr pat.pat_type)
                      ty
                  in
                  YALO.warn ~loc:pat.pat_loc ~file ~linter w ~msg
              | None -> ()
        in
        traverse.pat <- (linter, { version_pat } ) ::
                        traverse.pat
      )
