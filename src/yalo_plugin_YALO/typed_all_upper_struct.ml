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

open Yalo.V1
open Yalo_plugin_ocaml.V1

let lint_msg = "Inner modules should be full uppercase"

open OCAML_TAST

[%%if ocaml_version < (4, 10, 0)]
let module_expr check ~file ~linter me =
  match me.mod_desc with
  | Tmod_functor (_id, name_op_loc, Some mty, _) ->
     check ~file ~linter name_op_loc mty.mty_type
  | _ -> ()

let module_type check ~file ~linter mty =
  match mty.mty_desc with
  | Tmty_functor (_id, name_op_loc, Some mty, _) ->
     check ~file ~linter name_op_loc mty.mty_type
  | _ -> ()
[%%else]
let module_expr check ~file ~linter me =
  match me.mod_desc with
  | Tmod_functor (Named (_id, name_op_loc, mty), _) ->
     check ~file ~linter name_op_loc mty.mty_type
  | _ -> ()

let module_type check ~file ~linter mty =
  match mty.mty_desc with
  | Tmty_functor (Named (_id, name_op_loc, mty), _) ->
     check ~file ~linter name_op_loc mty.mty_type
  | _ -> ()
[%%endif]

let register ns
      ?(name="all_upper_struct")
      ~tags
      ?(msg = lint_msg)
      id
  =
  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~msg
  in

  OCAML_LANG.new_tast_impl_traverse_linter ns
    ("check:typed:" ^ YALO_WARNING.name w)
    ~warnings:[ w ]
    OCAML_TAST.(fun ~file:_ ~linter traverse ->

    let check ~file ~linter mb_name mty =
      match (OCAML_TAST.module_binding_name mb_name).Location.txt with
      | None -> ()
      | Some name ->
         let upname = String.uppercase_ascii name in
         if upname <> name then
           match mty with
           | Types.Mty_signature _ ->
              let loc = mb_name.loc in
              YALO.warn ~loc ~file ~linter w
                ~msg:(Printf.sprintf
                        "Inner modules should be fully uppercase \
                         (%S here)"
                        upname)

           | Mty_ident _id -> ()
           | Mty_functor _ -> ()

           (* Aliases should follow this rule too, but only for user
              defined aliases *)
           | Mty_alias _ -> ()
    in

    let module_binding ~file ~linter mb =
      check ~file ~linter mb.mb_name mb.mb_expr.mod_type
    in
    let expression ~file ~linter exp =
      match exp.exp_desc with
      | Texp_letmodule (_mb_id, mb_name, _mb_presence, mb_expr, _exp) ->
         check ~file ~linter mb_name mb_expr.mod_type
      | _ -> ()
    in
    traverse.module_binding <- (linter, module_binding) ::
                                 traverse.module_binding ;
    traverse.expr <- (linter, expression) ::
                       traverse.expr ;
    traverse.module_expr <- (linter, module_expr check) ::
                              traverse.module_expr ;
    traverse.module_type <- (linter, module_type check) ::
                              traverse.module_type ;
  )
