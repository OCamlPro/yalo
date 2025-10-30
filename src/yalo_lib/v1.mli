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

module YALOTYPES : sig

  type plugin
  type namespace
  type warning
  type tag
  type file
  type project
  type config_section
  type language
  type file_kind
  type linter
  type document
  type folder
  
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

  type 'linter_entry new_gen_linter =
    namespace ->
    string ->
    warnings:warning list ->
    ?on_begin : (unit -> unit) ->
    ?on_open : (file:file -> unit) ->
    ?on_close : (file:file -> unit) ->
    ?on_end : (unit -> unit) ->
    (file:file -> 'linter_entry -> unit) -> unit

end

open YALOTYPES

module YALO : sig

  val verbose : int -> bool 
  val new_plugin : ?version:string ->
                   ?args:(string list * Ezcmd.V2.EZCMD.spec *
                            Ezcmd.V2.EZCMD.TYPES.info)
                   list ->
                   string -> plugin
  val add_plugin_args : plugin ->
    (string list * Ezcmd.V2.EZCMD.spec * Ezcmd.V2.EZCMD.TYPES.info)
      list -> unit

  (* A plugin must be all in the character set [A-Z0-9_],
     starting with a letter. *)
  val new_namespace : plugin -> string -> namespace
  (* A tag must be all in the character set [a-z0-9_],
     starting with a letter. *)
  val new_tag : string -> tag
  val new_warning :
    namespace ->
    ?tags:tag list ->
    ?desc:string ->
    name:string -> msg:string -> int -> warning
  val tag_danger : tag

  val warn : location -> file:file -> ?msg:string ->
             ?autofix:(YALOTYPES.location * string) list ->
             warning -> unit

  val file_name : file:file -> string
  val doc_name : file_doc:document -> string
  val mkloc :
    bol:int ->
    ?start_cnum:int ->
    ?end_cnum:int -> lnum:int -> file:file ->
    unit -> location

  module CONFIG : sig
    val create_section : plugin -> short_help:string -> config_section

    (* Use EzConfig.OP to manipulate options *)
    val create_option :
      config_section ->
      path:string list ->
      short_help:string ->
      ?long_help:string list ->
      ?level:int ->
      'a EzConfig.option_class ->
      'a -> 'a EzConfig.config_option
  end

  module STORE : sig
    type 'a t
    val create : plugin -> 'a t
    val put : 'a t -> file -> 'a -> unit
    val check : 'a t -> file -> 'a option
    val get : 'a t -> file -> 'a
  end
end

module YALOLANG : sig

  val new_language : plugin -> string -> language
  val new_file_kind :
    lang:language ->
    ?exts:string list ->
    name:string ->
    ?validate:(file_doc:document -> bool) ->
    lint:(file:file -> unit) ->
    unit ->
    file_kind

  val new_linter :
    language ->
    namespace ->
    string ->
    warnings:warning list ->
    ?on_begin:(unit -> unit) ->
    ?on_open:(file:file -> unit) ->
    ?on_close:(file:file -> unit) ->
    ?on_end:(unit -> unit) ->
    (linter -> unit) -> unit

  val new_gen_linter :
    language ->
    (linter * 'a) list ref ->
    YALOTYPES.namespace ->
    string ->
    warnings:YALOTYPES.warning list ->
    ?on_begin:(unit -> unit) ->
    ?on_open:(file:YALOTYPES.file -> unit) ->
    ?on_close:(file:YALOTYPES.file -> unit) ->
    ?on_end:(unit -> unit) -> 'a -> unit

    val filter_linters :
           file:file ->
           (linter * 'a) list -> (linter * 'a) list
    val lint_with_active_linters :
      (linter * (file:file -> 'a -> unit)) list ref ->
      file:file -> 'a -> unit
     val iter_linters_open :
       file:YALOTYPES.file -> (linter * 'a) list -> unit
     val iter_linters_close :
       file:YALOTYPES.file -> (linter * 'a) list -> unit
    val iter_linters :
      file:file ->
      (linter * (file:file -> 'a -> unit)) list ->
      'a -> unit

    (*
    module FS : sig
      type t
      val get_folder : t -> string -> folder
    end

    module FOLDER : sig
      type t = folder
      val root : t -> FS.t
      val project : t -> project
      val parent : t -> t option
      val basename : t -> string
      val name : t -> string

      val set_project : t -> project -> unit
    end

    module DOCUMENT : sig
      type t = document
      val parent : t -> folder
      val basename : t -> string
      val name : t -> string
      end
      
     val add_file_classifier :
       (file_doc:YALOTYPES.document -> YALOTYPES.file_kind option) ->
       unit
     val add_folder_updater : (folder:YALOTYPES.folder -> unit) -> unit
     *)


end

module YALO_INTERNAL : sig

  val add_file :
    file_doc:Types.document ->
    ?file_kind:YALOTYPES.file_kind ->
    unit ->
    unit
  val new_file :
    file_doc:Types.document ->
    file_kind:file_kind ->
    file_crc:Digest.t -> string -> file
  val get_document : Types.folder -> string -> Types.document

end

val init : unit -> unit
