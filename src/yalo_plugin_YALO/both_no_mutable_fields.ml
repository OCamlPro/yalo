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

let lint_msg = "Mutable fields in records are forbidden in this project"

(* TODO: we should extend this to classes too *)

let register ns
    ?(name="no_mutable_fields")
    ~tags
    ?(msg = lint_msg)
    id
  =
  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~set_by_default:false (* disabled by default *)
      ~msg
  in
  if OCAML_LANG.is_running_as_ppx () then
    OCAML_LANG.new_ast_impl_traverse_linter ns
      ("check:typed:" ^ YALO_WARNING.name w)
      ~warnings:[ w ]
      OCAML_AST.(fun ~file:_ ~linter traverse ->
          let label_declaration ~file ~linter pld =
            match pld.pld_mutable with
            | Mutable ->
                let loc = pld.pld_loc in
                YALO.warn ~loc ~file ~linter w
            | Immutable -> ()
          in
          traverse.label_declaration <- (linter, label_declaration) ::
                                        traverse.label_declaration
        )
  else
    OCAML_LANG.new_tast_impl_traverse_linter ns
      ("check:typed:" ^ YALO_WARNING.name w)
      ~warnings:[ w ]
      OCAML_TAST.(fun ~file ~linter traverse ->
          let label_declaration ld =
            match ld.ld_mutable with
            | Mutable ->
                let loc = ld.ld_loc in
                YALO.warn ~loc ~file ~linter w
            | Immutable -> ()
          in
          let type_declaration ~file:_ ~linter:_ t =
            match t.typ_kind with
            | Ttype_record labels ->
                List.iter label_declaration labels
            | Ttype_abstract
            | Ttype_open -> ()
            | Ttype_variant constructors ->
                List.iter (fun cd ->
                    match cd.cd_args with
                    | Cstr_tuple _ -> ()
                    | Cstr_record labels ->
                        List.iter label_declaration labels
                  ) constructors
          in
          traverse.type_declaration <- (linter, type_declaration) ::
                                       traverse.type_declaration
        )
