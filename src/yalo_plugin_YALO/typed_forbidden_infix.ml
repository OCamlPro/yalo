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

let lint_msg = "Use of infix operator not in the allowed set"

(* This list is stored in the allowed_stdlib_infix configuration
   variable for user customization *)
let default_stdlib_infix =
  [
    "^" ;
    "+"; "-"; "*" ; "/" ;
    "="; "=="; "!="; ">"; "<"; "<="; ">=" ; "<>" ;
    "||" ; "&&" ;
    ":="; "!";
    "@" ;
    "@@"; "|>"
  ]

let register ns section
    ?(name="forbidden_infix")
    ~tags
    ?(msg = lint_msg)
    id
  =
  let ns_name = YALO_NS.name ns in
  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~msg
  in

  let allowed_infix =
    YALO.CONFIG.create_option section
      ~path:[ ns_name; "allowed_infix"]
      ~short_help:"List of infix operators allowed in this project \
                   (such as [ \"++\", \"--\" ]) for the \
                   forbidden_infix warning"
      (EzConfig.list_option EzConfig.string_option)
      []
  in
  let allowed_stdlib_infix =
    YALO.CONFIG.create_option section
      ~path:[ ns_name; "allowed_stdlib_infix"]
      ~short_help:"List of infix operators from Stdlib allowed in this \
                   project (missing operators should be reported \
                   upstream)"
      (EzConfig.list_option EzConfig.string_option)
      default_stdlib_infix
  in
  let allowed_infix_operators =
    StringSet.of_list (!!allowed_infix
                       @
                       List.map (fun s ->
                           "Stdlib." ^ s)
                         !!allowed_stdlib_infix) in

  let check_infix ~file ~linter path loc =
    let name = Path.name path in
    if not (StringSet.mem name allowed_infix_operators) then
      YALO.warn ~loc ~file ~linter w
        ~msg:(Printf.sprintf
                "Infix operator %S is not in not \
                 allowed (add it to YALO { \
                 allowed_infix } otherwise)"
                name)
  in
  OCAML_LANG.new_tast_impl_traverse_linter ns
    ("check:typed:" ^ YALO_WARNING.name w)
    ~warnings:[ w ]
    OCAML_TAST.(fun ~file ~linter traverse ->
        let check_expr ~file:_ ~linter:_ expr =
          match expr.exp_desc with
          | Texp_ident (p, longident, _) ->
              begin
                match Longident.flatten longident.txt with
                | [ "*sth*" | "*opt*" ] -> ()
                | [ name ] when
                    begin
                      match name.[0] with
                      | 'a'..'z' | '_' -> false
                      | _ -> true
                    end
                  ->
                    check_infix ~file ~linter p expr.exp_loc
                | _ -> ()
              end
          | _ -> ()
        in
        let binding_op ~file ~linter bo =
          check_infix ~file ~linter bo.bop_op_path bo.bop_loc
        in
        traverse.expression <- (linter, check_expr) :: traverse.expression ;
        traverse.binding_op <- (linter, binding_op) :: traverse.binding_op

      )
