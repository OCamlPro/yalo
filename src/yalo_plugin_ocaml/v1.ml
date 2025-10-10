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

module OCAMLLANG = struct

  let ocaml = Engine.ocaml

  let ml_file = YALOLANG.new_file_kind ocaml ~exts:[ "ml" ]
                  "OCaml Implementation Source File"
                  Engine.check_impl_source
  let mli_file = YALOLANG.new_file_kind ocaml ~exts:[ "mli" ]
                   "OCaml Interface Source File"
                   Engine.check_intf_source
  let cmi_file = YALOLANG.new_file_kind ocaml ~exts:[ "cmi" ]
                   "OCaml Interface Compiled File"
                   Engine.check_cmi
  let cmti_file = YALOLANG.new_file_kind ocaml ~exts:[ "cmti" ]
                    "OCaml Interface Typedtree File"
                    Engine.check_cmt
  let cmt_file = YALOLANG.new_file_kind ocaml ~exts:[ "cmt" ]
                   "OCaml Implementation Typedtree File"
                   Engine.check_cmt

  let new_src_file_linter ns name ~warnings
        ?on_begin ?on_end f =
    Engine.new_src_file_linter ns name ~warnings ?on_begin ?on_end f


  let new_src_content_linter = Engine.new_src_content_linter
  let new_src_line_linter = Engine.new_src_line_linter
  let new_sig_linter = Engine.new_sig_linter
  let new_ast_intf_linter = Engine.new_ast_intf_linter
  let new_ast_impl_linter = Engine.new_ast_impl_linter
  let new_tast_intf_linter = Engine.new_tast_intf_linter
  let new_tast_impl_linter = Engine.new_tast_impl_linter

end
