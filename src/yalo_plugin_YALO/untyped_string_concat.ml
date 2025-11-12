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

open OCAML_AST

let lint_msg =
  {|(a ^ b ^ c) should be replaced by Printf.sprintf "%s%s%s" a b c|}

let register ns
    ?(name="string_concat")
    ~tags
    ?(msg = lint_msg)
    id
  =
  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~msg
  in

  OCAML_LANG.new_ast_impl_traverse_linter ns
    ("check:" ^ name)
    ~warnings:[ w ]
    (fun ~file:_ ~linter traverse ->
       let expression ~file ~linter e =
         match e.pexp_desc with
         | Pexp_apply(
             { pexp_desc = Pexp_ident { txt = Lident "^"; _ } ; _ },
             [ _, _ ;
               _,
               { pexp_desc = Pexp_apply (
                     { pexp_desc = Pexp_ident { txt = Lident "^" ; _ } ; _ },
                     _ ) ; _ } ]
           ) ->
             YALO.warn ~loc:e.pexp_loc ~file ~linter w
         | _ -> ()
       in
       traverse.expression <- (linter, expression) :: traverse.expression
    );
  ()
