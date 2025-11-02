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

open Types

module YALO_OP = Yalo_misc.Utils.OP

module YALO_TYPES = struct

  type plugin = Types.plugin
  type language = Types.language
  type namespace = Types.namespace
  type file = Types.file
  type warning = Types.warning
  type tag = Types.tag
  type project = Types.project
  type config_section = EzConfig.config_section
  type linter = Types.linter
  type file_kind = Types.file_kind
  type document = Types.document
  type folder = Types.folder
  type filepath = Types.filepath
  type fs = Types.fs

  type position = Lexing.position = {
      pos_fname : string;
      pos_lnum : int;
      pos_bol : int;
      pos_cnum : int;
    }

  type location = Location.t = {
      loc_start: position;
      loc_end: position;
      loc_ghost: bool;
    }

  type ('linter_input, 'linter_output) linter_function =
    file:file -> linter:linter -> 'linter_input -> 'linter_output

  type ('linter_input, 'linter_output) new_gen_linter =
    namespace ->
    string ->
    warnings:warning list ->
    ?on_begin : (unit -> unit) ->
    ?on_open : (file:file -> linter:linter -> unit) ->
    ?on_close : (file:file -> linter:linter -> unit) ->
    ?on_end : (unit -> unit) ->
              ('linter_input, 'linter_output) linter_function ->
              unit

  type 'linter_input new_gen_unit_linter =
    ('linter_input,unit) new_gen_linter

  type ('a,'b) active_linters =
    ( linter * ('a, 'b) linter_function ) list

  type src_line_input = Types.src_line_input = {
      line_loc : location ;
      line_line : string ;
      line_sep : string ;
    }

  type src_file_input = Types.src_file_input = {
      file_loc : location ;
    }

  type src_content_input = Types.src_content_input = {
      content_loc : location ;
      content_string : string ;
    }

end

module YALO = struct

  let new_plugin = Engine.new_plugin
  let add_plugin_args = Engine.add_plugin_args

  let new_namespace = Engine.new_namespace
  let new_tag = Engine.new_tag
  let new_warning = Engine.new_warning

  let tag_danger = new_tag "danger"
  let warn = Engine.warn

  let file_name ~file = file.file_name
  let doc_name ~file_doc = file_doc.doc_name

  let mkloc = Engine.mkloc

  module CONFIG = struct
    let create_section = Config.create_config_section
    let create_option = Config.create_config_option
  end

  module STORE = File_store

  let verbose = Engine.verbose
end

module YALO_LANG = struct

  let new_language = Engine.new_language
  let new_file_kind = Engine.new_file_kind
  let new_linter = Engine.new_linter

  (* utils *)
  let new_gen_linter = Engine.new_gen_linter
  let filter_linters = Engine.filter_linters
  let lint_with_active_linters = Engine.lint_with_active_linters
  let iter_linters_open = Engine.iter_linters_open
  let iter_linters_close = Engine.iter_linters_close
  let iter_linters = Engine.iter_linters

  let add_file_classifier = Engine.add_file_classifier
  let add_folder_updater = Engine.add_folder_updater

  let update_warnings = Engine.update_warnings
end

module YALO_FS = struct
  (*      let get_folder : fs -> string -> folder *)
  let folder fs = fs.fs_folder
end

module YALO_FOLDER = struct
  let name folder = folder.folder_name
  let fs folder = folder.folder_fs
  let projects folder = folder.folder_projects
  let folders folder = folder.folder_folders

  (* modifier *)
  let set_projects folder p = folder.folder_projects <- p
end

module YALO_DOC = struct
(*
  val parent : t -> folder
  val basename : t -> string
  val name : t -> string
 *)
end

module YALO_INTERNAL = struct
  let new_file = Engine.new_file (* don't use *)
  let add_file = Engine.add_file
  let get_document = Engine.get_document
end

let init () = ()

