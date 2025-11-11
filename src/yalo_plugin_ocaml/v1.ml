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


module OCAML_LEX = struct
  include Parser
  let get_STRING = Main.get_STRING
end

module OCAML_AST = struct
  include Ast_traverse.OCAML_AST
  module OCAML_TRAVERSE = Ast_traverse.OCAML_AST_TRAVERSE
  module OCAML_CHECK = Ast_check
end

module OCAML_TAST = struct
  include Tast_traverse.OCAML_TAST
  module OCAML_TRAVERSE = Tast_traverse.OCAML_TAST_TRAVERSE
end


module OCAML_LANG = struct

  let ocaml = Main.ocaml

  let ml_file = YALO_LANG.new_file_kind
      ~lang:ocaml
      ~exts:[ "ml" ]
      ~name:"OCaml Implementation Source File"
      ~validate:Main.check_in_source_dir
      ~lint:Main.check_impl_source
      ()
  let mli_file = YALO_LANG.new_file_kind
      ~lang:ocaml
      ~exts:[ "mli" ]
      ~name:"OCaml Interface Source File"
      ~validate:Main.check_in_source_dir
      ~lint:Main.check_intf_source
      ()
  let cmi_file = YALO_LANG.new_file_kind
      ~lang:ocaml
      ~exts:[ "cmi" ]
      ~name:"OCaml Interface Compiled File"
      ~validate:Main.check_in_artefact_dir
      ~lint:Main.check_cmi
      ()
  let cmti_file = YALO_LANG.new_file_kind
      ~lang:ocaml
      ~exts:[ "cmti" ]
      ~name:"OCaml Interface Typedtree File"
      ~validate:Main.check_in_artefact_dir
      ~lint:Main.check_cmt
      ()
  let cmt_file = YALO_LANG.new_file_kind
      ~lang:ocaml
      ~exts:[ "cmt" ]
      ~name:"OCaml Implementation Typedtree File"
      ~validate:Main.check_in_artefact_dir
      ~lint:Main.check_cmt
      ()

  let new_src_file_linter ns name ~warnings
      ?on_begin ?on_end f =
    Main.new_src_file_linter ns name ~warnings ?on_begin ?on_end f

  let new_src_lex_linter = Main.new_src_lex_linter
  let new_src_content_linter = Main.new_src_content_linter
  let new_src_line_linter = Main.new_src_line_linter
  let new_sig_linter = Main.new_sig_linter
  let new_ast_intf_linter = Main.new_ast_intf_linter
  let new_ast_impl_linter = Main.new_ast_impl_linter
  let new_tast_intf_linter = Main.new_tast_intf_linter
  let new_tast_impl_linter = Main.new_tast_impl_linter

  let new_ast_impl_traverse_linter = Main.new_ast_impl_traverse_linter
  let new_ast_intf_traverse_linter = Main.new_ast_intf_traverse_linter

  let new_tast_impl_traverse_linter = Main.new_tast_impl_traverse_linter
  let new_tast_intf_traverse_linter = Main.new_tast_intf_traverse_linter

  let is_menhir_generated_file = Main.is_menhir_generated_file

  let () =
    YALO_LANG.add_folder_updater Main.folder_updater ;
    ()
end
