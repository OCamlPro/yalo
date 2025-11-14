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

type 'a warning_config = {
  w_string_concat : 'a option ;
  w_incr_decr : 'a option ;
  w_comp_boolean : 'a option ;
  w_failwith_sprintf : 'a option ;
  w_list_length_comp_zero : 'a option ;
}

let msg_string_concat =
  {|(a ^ b ^ c) should be replaced by Printf.sprintf "%s%s%s" a b c|}
let msg_incr_decr =
  {|"x := !x +/- 1" should be replaced by "incr/decr x"|}
let msg_comp_boolean =
  {|comparison with a boolean should always be simplified|}
let msg_failwith_sprintf =
  "\"failwith (sprintf [...])\" should be replaced by \
   \"Printf.ksprintf failwith [...]\""
let msg_list_length_comp_zero =
  {|"String.length l =/<> 0" should be replaced by "l =/<> []"|}

let register ns
    ~tags
    config
  =
  let warnings = ref [] in
  let some x =
    warnings := x :: !warnings ;
    Some x
  in
  let w_string_concat =
    match config.w_string_concat with
    | None -> None
    | Some id -> some @@
        YALO.new_warning ns ~name:"string_concat"
          id ~tags ~msg: msg_string_concat
  in

  let w_incr_decr =
    match config.w_incr_decr with
    | None -> None
    | Some id -> some @@
        YALO.new_warning ns ~name:"incr_decr"
          id ~tags ~msg: msg_incr_decr
  in

  let w_comp_boolean =
    match config.w_comp_boolean with
    | None -> None
    | Some id -> some @@
        YALO.new_warning ns ~name:"comp_boolean"
          id ~tags ~msg: msg_comp_boolean
  in

  let w_failwith_sprintf =
    match config.w_failwith_sprintf with
    | None -> None
    | Some id -> some @@
        YALO.new_warning ns ~name:"failwith_sprintf"
          id ~tags ~msg: msg_failwith_sprintf
  in

  let w_list_length_comp_zero =
    match config.w_list_length_comp_zero with
    | None -> None
    | Some id -> some @@
        YALO.new_warning ns ~name:"list_length_comp_zero"
          id ~tags ~msg: msg_list_length_comp_zero
  in

  let config = {
    w_string_concat ;
    w_incr_decr ;
    w_comp_boolean ;
    w_failwith_sprintf ;
    w_list_length_comp_zero ;
  } in

  OCAML_LANG.new_ast_impl_traverse_linter ns
    "check:string_concat"
    ~warnings:!warnings
    (fun ~file:_ ~linter traverse ->

       let expression ~file ~linter e =
         match config, e.pexp_desc with

         (* detect ( _ ^ _ ^ _ ) *)
         | { w_string_concat = Some w ; _},
           Pexp_apply(
             { pexp_desc = Pexp_ident { txt = Lident "^"; _ } ; _ },
             [ _, _ ;
               _,
               { pexp_desc = Pexp_apply (
                     { pexp_desc = Pexp_ident { txt = Lident "^" ; _ } ; _ },
                     _ ) ; _ } ]
           ) ->
             YALO.warn ~loc:e.pexp_loc ~file ~linter w

         (* detect x := !x + 1 ---> incr x *)
         |  { w_incr_decr = Some w ; _},
            Pexp_apply (
              { pexp_desc = Pexp_ident { txt = Lident ":=";_};_},
              [
                Nolabel,
                { pexp_desc = Pexp_ident var1;_} ;
                Nolabel,
                { pexp_desc = Pexp_apply (
                      { pexp_desc = Pexp_ident
                            { txt = Lident (( "+" | "-") as op);_};_},
                      (
                        [
                          Nolabel,
                          { pexp_desc = Pexp_apply (
                                { pexp_desc = Pexp_ident
                                      { txt = Lident "!";_};_},
                                [
                                  Nolabel,
                                  { pexp_desc = Pexp_ident var2;_} ]
                              );_};
                          Nolabel,
                          { pexp_desc =
                              Pexp_constant
                                (Pconst_integer ("1", None));_}
                        ]
                      |
                        [
                          Nolabel,
                          { pexp_desc =
                              Pexp_constant
                                (Pconst_integer ("1", None));_};
                          Nolabel,
                          { pexp_desc = Pexp_apply (
                                { pexp_desc = Pexp_ident
                                      { txt = Lident "!";_};_},
                                [
                                  Nolabel,
                                  { pexp_desc = Pexp_ident var2;_} ]
                              );_};
                        ]
                      ));_}
              ]) when var1.txt = var2.txt ->
             let msg = Printf.sprintf
                 {|"x := !x %s 1" should be replaced by "%s x"|}
                 op (if op = "+" then "incr" else "decr")
             in
             YALO.warn ~loc:e.pexp_loc ~file ~linter w ~msg

         | { w_comp_boolean = Some w ; _},
           Pexp_apply (
             { pexp_desc =
                 Pexp_ident {
                   txt = Lident ( "=" | "<>" | "==" | "!=" );_};_},
             (
               [
                 Nolabel, _ ;
                 Nolabel,
                 { pexp_desc =
                     Pexp_construct ({ txt = Lident ("false"|"true");_},_);_}
               ]
             | [
               Nolabel,
               { pexp_desc =
                   Pexp_construct ({ txt = Lident ("false"|"true");_},_);_};
               Nolabel, _ ;
             ])) ->
             YALO.warn ~loc:e.pexp_loc ~file ~linter w

         | { w_failwith_sprintf = Some w ; _},
           Pexp_apply (
             { pexp_desc =
                 Pexp_ident {
                   txt = Lident "failwith" ;_};_},
             [
               Nolabel,
               { pexp_desc = Pexp_apply (
                     { pexp_desc = Pexp_ident
                           { txt = lid; _};_},
                     _ );_}
             ]) when OCAML_AST.longident_name lid = "Printf.sprintf" ->
             YALO.warn ~loc:e.pexp_loc ~file ~linter w

         | { w_list_length_comp_zero = Some w ; _},
           Pexp_apply (
             { pexp_desc =
                 Pexp_ident {
                   txt = Lident ( "=" | "<>" | "==" | "!=" ) ;_};_},
             [
               Nolabel,
               { pexp_desc =
                   Pexp_apply (
                     { pexp_desc = Pexp_ident
                           { txt = lid_list_length;_}; _},
                     _);_} ;
               Nolabel,
               { pexp_desc =
                   Pexp_constant
                     (Pconst_integer ("0", None));_};
             ])
           when
             OCAML_AST.longident_name lid_list_length = "List.length" ->
             YALO.warn ~loc:e.pexp_loc ~file ~linter w

         | _ -> ()
       in
       traverse.expression <- (linter, expression) :: traverse.expression
    );
  ()
