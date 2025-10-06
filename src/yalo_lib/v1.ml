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

module YALO : sig

  type plugin
  type warning
  type tag
  type file
  type project

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

  type src_line_input = {
      line_loc : location ;
      line_line : string ;
      line_sep : string ;
    }

  type src_file_input = {
      file_loc : location ;
    }

  type src_content_input = {
      content_loc : location ;
      content_string : string ;
    }


  (* A plugin must be all in the character set [A-Z0-9_],
     starting with a letter. *)
  val new_plugin : ?version:string -> string -> plugin
  (* A tag must be all in the character set [a-z0-9_],
     starting with a letter. *)
  val new_tag : string -> tag
  val new_warning :
    plugin ->
    ?tags:tag list ->
    ?desc:string ->
    name:string -> msg:string -> int -> warning
  val tag_danger : tag

  val new_src_file_linter :
    plugin ->
    string ->
    warnings:warning list ->
    ?on_begin : (unit -> unit) ->
    ?on_end : (unit -> unit) ->
              (file:file -> src_file_input -> unit) -> unit

  type 'linter_entry new_gen_linter =
    plugin ->
    string ->
    warnings:warning list ->
    ?on_begin : (unit -> unit) ->
    ?on_open : (file:file -> unit) ->
    ?on_close : (file:file -> unit) ->
    ?on_end : (unit -> unit) ->
    (file:file -> 'linter_entry -> unit) -> unit

  val new_src_content_linter : src_content_input new_gen_linter
  val new_src_line_linter : src_line_input new_gen_linter
  val new_sig_linter : Cmi_format.cmi_infos new_gen_linter
  val new_ast_intf_linter :  Ppxlib.Parsetree.signature new_gen_linter
  val new_ast_impl_linter :  Ppxlib.Parsetree.structure new_gen_linter
  val new_tast_intf_linter : Typedtree.signature new_gen_linter
  val new_tast_impl_linter : Typedtree.structure new_gen_linter

  val warn : location -> file:file -> ?msg:string -> warning -> unit

  val file_name : file:file -> string
  val mkloc :
    bol:int ->
    ?start_cnum:int ->
    ?end_cnum:int -> lnum:int -> file:file ->
    unit -> location

  module STORE : sig
    type 'a t
    val create : plugin -> 'a t
    val put : 'a t -> file -> 'a -> unit
    val check : 'a t -> file -> 'a option
    val get : 'a t -> file -> 'a
  end

  module CONFIG : sig
    type section
    val create_section : string -> short_help:string -> section

    (* Use EzConfig.OP to manipulate options *)
    val create_option :
      section ->
      path:string list ->
      short_help:string ->
      ?long_help:string list ->
      ?level:int ->
      'a EzConfig.option_class ->
      'a -> 'a EzConfig.config_option
  end

end = struct

  open Types

  type file = Types.file
  type plugin = Types.plugin
  type warning = Types.warning
  type tag = Types.tag
  type project = Types.project

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

  type 'linter_entry new_gen_linter =
    plugin ->
    string ->
    warnings:warning list ->
    ?on_begin : (unit -> unit) ->
    ?on_open : (file:file -> unit) ->
    ?on_close : (file:file -> unit) ->
    ?on_end : (unit -> unit) ->
    (file:file -> 'linter_entry -> unit) -> unit

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

  let new_plugin = Engine.new_plugin
  let new_tag = Engine.new_tag
  let new_warning = Engine.new_warning

  let tag_danger = new_tag "danger"

  let new_src_file_linter = Engine.new_src_file_linter
  let new_src_line_linter = Engine.new_src_line_linter
  let new_src_content_linter = Engine.new_src_content_linter
  let new_sig_linter = Engine.new_sig_linter
  let new_ast_intf_linter = Engine.new_ast_intf_linter
  let new_ast_impl_linter = Engine.new_ast_impl_linter
  let new_tast_intf_linter = Engine.new_tast_intf_linter
  let new_tast_impl_linter = Engine.new_tast_impl_linter

  let warn = Engine.warn

  let file_name ~file = file.file_name

  let mkloc = Engine.mkloc

  module CONFIG = struct
    type section = EzConfig.config_section
    let create_section = Config.create_config_section
    let create_option = Config.create_config_option
  end
  module STORE = File_store
end

let init () = ()
