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

let plugin = YALO.new_plugin "yalo_plugin_YALO" ~version:"0.1.0"

let ns = YALO.new_namespace plugin "YALO"

let tag_untyped = YALO.new_tag "untyped"
let tag_typed = YALO.new_tag "typed"
let tag_lex = YALO.new_tag "lex"
let tag_immutable = YALO.new_tag "immutable"

let section = YALO.CONFIG.create_section
    plugin ~short_help:"YALO plugin"

let () =
  Line_linters.register ns section
    { w_id_line_too_long = 1 ;
      w_id_spaces_at_end = 2 ;
      w_id_tab_used =3 ;
      w_id_non_printable_char = 4 ;
      w_id_no_final_newline =5 ;
      w_id_windows_newline = 6 ;
    } ;
  let w_use_obj =
    Untyped_use_obj.register ns
      ~tags:[ tag_untyped ] 7 in
  Untyped_use_external.register ns
    ~tags:[ tag_untyped ] 8 ;
  Typed_use_obj.register ns w_use_obj ;
  Typed_unqualified_id.register ns
    ~tags:[ tag_typed ] 9 ;
  Typed_all_upper_struct.register ns
    ~tags:[ tag_typed ] 10;
  Lex_in_one_pass.register ns
    ~tags:[ tag_lex ] {
    w_no_semisemi = Some 11;
    w_begin_fun = Some 16 ;
    w_useless_paren = Some 21 ;
  };
  Typed_forbidden_infix.register ns section
    ~tags: [ tag_typed ] 12;
  Typed_no_mutable_fields.register ns
    ~tags: [ tag_typed ; tag_immutable ] 13;
  Untyped_in_one_pass.register ns
    ~tags:[ tag_untyped ] {
    w_string_concat = Some 14 ;
    w_incr_decr = Some 18 ;
    w_failwith_sprintf = Some 19 ;
    w_comp_boolean = Some 20 ;
    w_list_length_comp_zero = Some 22 ;
    w_list_length_comp_any = Some 23 ;
    w_list_length_comp_list_length = Some 24 ;
    w_then_bool_else_bool = Some 25 ;
    w_then_or_else_bool = Some 26 ;
    w_else_unit = Some 27 ;
    w_try_catch_all = Some 28 ;
    w_list_append_for_one = Some 29 ;
    w_useless_sprintf = Some 30 ;
    w_suspicious_for_zero_to_len = Some 31 ;
  };
  Typed_fun_fun.register ns
    ~tags: [ tag_typed ] 15;
  Lex_paren_semi.register ns
    ~tags: [ tag_lex ] 17;
  (* next one: 32 *)
  ()
