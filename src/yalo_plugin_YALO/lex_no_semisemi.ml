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

let lint_msg = "Double semi (;;) should be avoided"

let register ns
    ?(name="no_semisemi")
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
       let rec iter tokens =
         match tokens with
         | (SEMISEMI, loc) :: tokens ->
             YALO.warn ~loc ~file ~linter w ;
             iter tokens
         | _ :: tokens -> iter tokens
         | [] -> ()
       in
       iter tokens
    )
