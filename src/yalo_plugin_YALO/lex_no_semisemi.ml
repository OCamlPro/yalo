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

type ids = {
  w_id_no_semisemi : int ;
  w_id_begin_fun : int ;
}

let register ns
    ~tags
    ids
  =
  let w_no_semi_semi =
    YALO.new_warning ns ~name:"no_semisemi" ids.w_id_no_semisemi
      ~tags
      ~msg:"Double semi (;;) should be avoided"
  in
  let w_begin_fun =
    YALO.new_warning ns ~name:"begin_fun" ids.w_id_begin_fun
      ~tags
      ~msg:
        {|"begin fun" is misleading because "begin" starts a sequence \
of instructions|}
  in

  OCAML_LANG.new_src_lex_linter ns
    "check:lex:warnings"
    ~warnings:[ w_no_semi_semi ; w_begin_fun ]
    (fun ~file ~linter tokens ->
       let rec iter tokens =
         match tokens with
         | (SEMISEMI, loc) :: _ ->
             YALO.warn ~loc ~file ~linter w_no_semi_semi ;
             iter2 tokens
         | (BEGIN, loc) :: ( (FUN | FUNCTION) , _) :: _ ->
             YALO.warn ~loc ~file ~linter w_begin_fun ;
             iter2 tokens
         | _ :: tokens -> iter tokens
         | [] -> ()

       and iter2 tokens = iter (List.tl tokens)
       in
       iter tokens
    )
