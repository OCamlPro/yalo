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

let plugin = YALO.new_plugin "yalo_plugin_ZANUDA" ~version:"0.1.0"

let ns = YALO.new_namespace plugin "ZANUDA"

let tag_line = YALO.new_tag "line"
let tag_autofix = YALO.new_tag "autofix"
let tag_untyped = YALO.new_tag "untyped"
let tag_typed = YALO.new_tag "typed"
let tag_fpcourse = YALO.new_tag "fpcourse"
let tag_readability = YALO.new_tag "readability"
let tag_camelot = YALO.new_tag "camelot"
let tag_suspicious = YALO.new_tag "suspicious"

let section = YALO.CONFIG.create_section
                plugin ~short_help:"ZANUDA plugin"

let w_id_caml_cased_types = 1
let w_id_no_toplevel_eval = 2
let w_id_camel_extra_dollar = 3
let w_id_format_module_usage = 4
let w_id_expect_tests_no_names = 5
let w_id_wrong_ignoring = 6
let w_id_var_should_not_be_used = 7
let w_id_ambiguous_constructor = 8

let () =
  Untyped_Casing.register ns
    ~tags:[ tag_untyped ; tag_fpcourse ; tag_readability ]
    ~id:w_id_caml_cased_types ;
  Untyped_Toplevel_Eval.register ns
    ~tags:[ tag_untyped ; tag_fpcourse ; tag_readability ]
    ~id:w_id_no_toplevel_eval ;
  Untyped_Dollar.register ns
    ~tags:[ tag_untyped ; tag_fpcourse ; tag_readability ]
    ~id:w_id_camel_extra_dollar ;
  Untyped_Expect_names.register ns
    ~tags:[ tag_untyped ; tag_fpcourse ; tag_readability ]
    ~id: w_id_expect_tests_no_names ;
  Typed_Printf.register ns
    ~tags:[ tag_typed ; tag_camelot ; tag_readability ]
    ~set_by_default:false
    ~id: w_id_format_module_usage ;
  Typed_Ignore.register ns
    ~tags:[ tag_typed ; tag_suspicious ; tag_fpcourse ]
    ~id: w_id_wrong_ignoring ;
  Typed_Var_should_not_be_used.register ns
    ~tags:[ tag_typed ; tag_suspicious ; tag_fpcourse ]
    ~id: w_id_var_should_not_be_used ;
  Typed_Ambiguous_constructors.register ns
    ~tags:[ tag_typed ; tag_suspicious ]
    ~id: w_id_ambiguous_constructor ;
  ()
