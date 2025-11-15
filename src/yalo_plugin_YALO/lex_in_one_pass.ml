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

(* This linter passes on all tokens, so we should avoid calling it too
   many times (i.e. instantiating such a linter for every
   warning). Let's gather here all lex linters. *)

type 'a config = {
  w_no_semisemi : 'a option ;
  w_begin_fun : 'a option ;
  w_useless_paren : 'a option ;
}

let msg_no_semisemi = "Double semi (;;) should be avoided"
let msg_begin_fun =
  {|"begin fun" is misleading because "begin" starts a sequence \
of instructions|}
let msg_useless_paren =
  "This expression does not need to be between parenthesis"

let register ns
    ~tags
    config
  =
  let warnings = ref [] in
  let some x =
    warnings := x :: !warnings ;
    Some x
  in

  let w_no_semisemi =
    match config.w_no_semisemi with
    | None -> None
    | Some id -> some @@
        YALO.new_warning ns ~name:"no_semisemi"
          id ~tags ~msg: msg_no_semisemi
  in

  let w_begin_fun =
    match config.w_begin_fun with
    | None -> None
    | Some id -> some @@
        YALO.new_warning ns ~name:"begin_fun"
          id ~tags ~msg: msg_begin_fun
  in

  let w_useless_paren =
    match config.w_useless_paren with
    | None -> None
    | Some id -> some @@
        YALO.new_warning ns ~name:"useless_paren"
          id ~tags ~msg: msg_useless_paren
  in

  let config = {
    w_no_semisemi ;
    w_begin_fun ;
    w_useless_paren ;
  } in

  OCAML_LANG.new_src_lex_linter ns
    "check:lex:warnings"
    ~warnings:!warnings
    (fun ~file ~linter tokens ->
       let rec iter tokens =
         match config, tokens with

         | { w_no_semisemi = Some w ;_},
           (SEMISEMI, loc) :: _ ->
             YALO.warn ~loc ~file ~linter w ;
             iter2 tokens

         | { w_begin_fun = Some w ;_},
           (BEGIN, loc) :: ( (FUN | FUNCTION) , _) :: _ ->
             YALO.warn ~loc ~file ~linter w ;
             iter2 tokens

         | { w_useless_paren = Some w ;_},
           (LPAREN, loc) ::
           ( (TRUE | FALSE | UNDERSCORE | STRING _)  , _) ::
           (RPAREN, _) ::
           _ ->
             YALO.warn ~loc ~file ~linter w ;
             iter2 tokens

         | _, _ :: tokens -> iter tokens
         | _, [] -> ()

       and iter2 tokens = iter (List.tl tokens)
       in
       iter tokens
    )
