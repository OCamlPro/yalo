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

open YALO_INFIX
(* open OCAML_LEX *)

let msg_forbidden_keyword =
  "Keyword is forbidden by project configuration"

let memo f =
  let cache = Hashtbl.create 11 in
  fun v ->
    match Hashtbl.find cache v with
    | x -> x
    | exception Not_found ->
        let x = f v in
        Hashtbl.add cache v x;
        x

let register ns section
    ~tags
    id
  =

  let ns_name = YALO_NS.name ns in
  let forbidden_keywords =
    YALO.CONFIG.create_option section
      ~path:[ ns_name; "forbidden_keywords"]
      ~short_help:"List of (space separated) forbidden keywords or tokens"
      YALO_CONFIG.string_list_option
      [ "or" ]
  in

  let w =
    YALO.new_warning ns ~name:"forbidden_keyword"
      id ~tags ~msg: msg_forbidden_keyword
  in

  OCAML_LANG.new_src_lex_linter ns
    "check:lex:forbidden_keyword"
    ~warnings:[w]
    (fun ~file ~linter tokens ->

       let forbidden_keywords =
         memo
           (fun keywords ->
              let h = Hashtbl.create 111 in
              List.iter (fun k ->
                  try
                    let tokens = OCAML_LEX.tokens_of_string k in
                    List.iter (fun (tok, _loc) ->
                        Hashtbl.add h tok ()) tokens
                  with
                  | _exn ->
                      Printf.eprintf
                        "Configuration warning in %S: unrecognized \
                         keyword in %S\n%!"
                        "forbidden_keywords" k
                ) keywords;
              h) !!forbidden_keywords
       in

       let rec iter tokens =
         match tokens with

         | (token, loc) :: tokens ->
             if Hashtbl.mem forbidden_keywords token then
               YALO.warn ~loc ~file ~linter w ;
             iter tokens

         | [] -> ()
       in
       iter tokens
    )
