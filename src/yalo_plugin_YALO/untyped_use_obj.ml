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

let lint_msg = "The use of the Obj module is dangerous"

let register ns
      ?(name="use_obj")
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
      let check_longident ~file ~linter (loc, l) =
        let rec check_longident l =
          match l with
            Lident s ->
             if s = "Obj" then
               YALO.warn ~loc ~file ~linter w
            | Ldot (l, _) ->
               check_longident l
            | Lapply (l1, l2) ->
               check_longident l1 ;
               check_longident l2 ;
        in
        check_longident l
      in
      traverse.longident <- (linter, check_longident) :: traverse.longident
    );

  w
    (* We export it to be also used by Typed_use_obj,
       a typed version of this one *)
