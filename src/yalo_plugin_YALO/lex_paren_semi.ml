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

open OCAML_LEX

let lint_msg =
  {|"( expr ; expr )" is misleading because "(" should not hide a side-effects|}

let register ns
    ?(name="paren_semi")
    ~tags
    ?(msg = lint_msg)
    id
  =
  let w =
    YALO.new_warning ns ~name id
      ~tags
      ~msg
  in

  OCAML_LANG.new_src_lex_linter ns
    ("check:lex:" ^ YALO_WARNING.name w)
    ~warnings:[ w ]
    (fun ~file ~linter tokens ->
       let rec iter tokens stack =
         match tokens with
         | (RPAREN, _loc) :: tokens ->
             let stack =
               match stack with
               | [] -> []
               | _ :: stack -> stack
             in
             iter tokens stack
         | (LPAREN, loc) :: tokens ->
             iter tokens ( (loc, ref false) :: stack )
         | (SEMI, _ ) :: tokens ->
             begin
               match stack with
               | (loc, r) :: _ when not !r ->
                   YALO.warn ~loc ~file ~linter w
               | _ -> ()
             end;
             iter tokens stack

         | (
           (
             FUN | FUNCTION | LBRACE | LBRACKET| TRY | MATCH
           ), _ ) :: tokens ->
             begin
               match stack with
               | [] -> ()
               | (_,r) :: _ -> if not !r then r := true
             end;
             iter tokens stack
         | _ :: tokens ->
             iter tokens stack
         | [] -> ()
       in
       iter tokens []
    )
