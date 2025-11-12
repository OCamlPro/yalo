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

let lint_msg = "All arguments of an immediate fun should be defined at \
                once"

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


let register ns
    ?(name="fun_fun")
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
    OCAML_TAST.(fun ~file ~linter traverse ->
        let expression ~file:_ ~linter:_ e =
          if check e then
            YALO.warn ~loc:e.exp_loc ~file ~linter w
        in
        traverse.expression <- (linter, expression) ::
                               traverse.expression
      )
