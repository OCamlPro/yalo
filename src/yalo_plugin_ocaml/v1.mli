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

open Yalo.V1.YALO_TYPES

module OCAML_LEX : sig
  include (module type of Parser)
  (* extract info from STRING token, version compatible *)
  val get_STRING : token -> string * string option
end

module OCAML_AST : (* = Ppxlib.Ast *)
  module type of Ast_traverse.OCAML_AST
module OCAML_AST_TRAVERSE :
  module type of Ast_traverse.OCAML_AST_TRAVERSE

module OCAML_TAST :
  module type of Tast_traverse.OCAML_TAST
module OCAML_TAST_TRAVERSE :
  module type of Tast_traverse.OCAML_TAST_TRAVERSE

module OCAML_AST_CHECK :
  module type of Ast_check

module OCAML_LANG : sig

  val ocaml : language

  val ml_file : file_kind
  val mli_file : file_kind
  val cmi_file : file_kind
  val cmt_file : file_kind
  val cmti_file : file_kind

  val new_src_file_linter :
    namespace ->
    string ->
    warnings:warning list ->
    ?on_begin : (unit -> unit) ->
    ?on_end : (unit -> unit) ->
    (src_file_input, unit) linter_function -> unit

  val new_src_lex_linter :
    (OCAML_LEX.token * location) list new_gen_unit_linter
  val new_src_content_linter : src_content_input new_gen_unit_linter
  val new_src_line_linter : src_line_input new_gen_unit_linter
  val new_sig_linter : Cmi_format.cmi_infos new_gen_unit_linter
  val new_ast_intf_linter :  OCAML_AST.signature new_gen_unit_linter
  val new_ast_impl_linter :  OCAML_AST.structure new_gen_unit_linter
  val new_tast_intf_linter : OCAML_TAST.signature new_gen_unit_linter
  val new_tast_impl_linter : OCAML_TAST.structure new_gen_unit_linter

  val new_ast_impl_traverse_linter : OCAML_AST_TRAVERSE.t new_gen_unit_linter
  val new_ast_intf_traverse_linter : OCAML_AST_TRAVERSE.t new_gen_unit_linter

  val new_tast_impl_traverse_linter : OCAML_TAST_TRAVERSE.t new_gen_unit_linter
  val new_tast_intf_traverse_linter : OCAML_TAST_TRAVERSE.t new_gen_unit_linter

end

