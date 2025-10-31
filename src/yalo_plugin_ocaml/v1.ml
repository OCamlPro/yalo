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

include Ast_traverse
include Tast_traverse

module OCAML_LANG = struct

  let ocaml = Engine.ocaml

  let ml_file = YALO_LANG.new_file_kind
                  ~lang:ocaml
                  ~exts:[ "ml" ]
                  ~name:"OCaml Implementation Source File"
                  ~validate:Engine.check_in_source_dir
                  ~lint:Engine.check_impl_source
                  ()
  let mli_file = YALO_LANG.new_file_kind
                   ~lang:ocaml
                   ~exts:[ "mli" ]
                   ~name:"OCaml Interface Source File"
                   ~validate:Engine.check_in_source_dir
                   ~lint:Engine.check_intf_source
                   ()
  let cmi_file = YALO_LANG.new_file_kind
                   ~lang:ocaml
                   ~exts:[ "cmi" ]
                   ~name:"OCaml Interface Compiled File"
                   ~validate:Engine.check_in_artefact_dir
                   ~lint:Engine.check_cmi
                   ()
  let cmti_file = YALO_LANG.new_file_kind
                    ~lang:ocaml
                    ~exts:[ "cmti" ]
                    ~name:"OCaml Interface Typedtree File"
                    ~validate:Engine.check_in_artefact_dir
                    ~lint:Engine.check_cmt
                    ()
  let cmt_file = YALO_LANG.new_file_kind
                   ~lang:ocaml
                   ~exts:[ "cmt" ]
                   ~name:"OCaml Implementation Typedtree File"
                   ~validate:Engine.check_in_artefact_dir
                   ~lint:Engine.check_cmt
                   ()

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

  let new_ast_impl_traverse_linter = Engine.new_ast_impl_traverse_linter
  let new_ast_intf_traverse_linter = Engine.new_ast_intf_traverse_linter

  let new_tast_impl_traverse_linter = Engine.new_tast_impl_traverse_linter
  let new_tast_intf_traverse_linter = Engine.new_tast_intf_traverse_linter

  let () =
    Yalo.Engine.add_folder_updater Engine.folder_updater
end
