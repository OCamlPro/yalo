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

open Yalo.V1.YALOTYPES

module OCAMLLANG : sig

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
    (file:file -> src_file_input -> unit) -> unit

  val new_src_content_linter : src_content_input new_gen_linter
  val new_src_line_linter : src_line_input new_gen_linter
  val new_sig_linter : Cmi_format.cmi_infos new_gen_linter
  val new_ast_intf_linter :  Ppxlib.Parsetree.signature new_gen_linter
  val new_ast_impl_linter :  Ppxlib.Parsetree.structure new_gen_linter
  val new_tast_intf_linter : Typedtree.signature new_gen_linter
  val new_tast_impl_linter : Typedtree.structure new_gen_linter

end

