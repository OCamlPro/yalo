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
   https://github.com/Kakadu/zanuda/blob/master/
                                  src/typed/Ambiguous_constructors.ml
   for comparison
*)

open EzCompat
open Yalo.V1
open Yalo_plugin_ocaml.V1

open OCAML_AST

let lint_id = "ambiguous_constructors"
let tags = [ "nursery" ]
let lint_msg =  "Constructor should not look like defaults"
let documentation =
  {|
### What it does

Checks if there are constructor names that hide default constructor names
from `Stdlib`, such as `Some`, `None`, `Error`, `Ok`.

### Why it is important

Shadowing names of default constructors may lead to name clashes within
toplevel.
Using custom constructors is recommended.

|}
  |> Stdlib.String.trim

let format_msg cd_name =
  Format.sprintf
    "Constructor %S of this type should not look like defaults"
    cd_name

let register ~id ~tags
    ?(lint_id=lint_id) ?(msg=lint_msg) ?(desc=documentation)
    ns =

  let w = YALO.new_warning ns id ~name:lint_id ~tags ~msg ~desc in

  let bad_cstr_names = [ "Some"; "None"; "Error"; "Ok" ] in
  let bad_cstr_names = StringSet.of_list bad_cstr_names in
  OCAML_LANG.new_tast_impl_traverse_linter
    ns ("check:" ^ lint_id)
    ~warnings:[w]
    OCAML_TAST.(fun ~file ~linter traverse ->
        let type_declaration ~file:_ ~linter:_ typ =
          match typ.typ_manifest with
          | Some ctyp when
              (match TYPES.get_desc ctyp.ctyp_type with
               | Tconstr (p, _, _) when
                   (match Path.name p with
                    | "Stdlib.result" | "option" -> true
                    | _ -> false)
                 -> true
               | _ -> false) -> ()
          | _ ->
              match typ.typ_kind with
              | Ttype_variant cds ->
                  List.iter (fun { cd_name ; cd_loc = loc ; _ } ->
                      let cd_name = cd_name.txt in
                      if StringSet.mem cd_name bad_cstr_names then
                        YALO.warn ~loc ~file ~linter w ~msg:(format_msg cd_name)
                    ) cds
              | _ -> ()
        in
        traverse.type_declaration <- (linter, type_declaration) ::
                                     traverse.type_declaration
      )
