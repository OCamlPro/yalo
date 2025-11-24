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

open EzCompat

module YALO_CONFIG = Yalo_misc.Ez_config.V1.EZCONFIG

module YALO_INFIX : sig
  val (//) : string -> string -> string
  val ( !! ) : 'a YALO_CONFIG.config_option -> 'a
  val ( =:= ) : 'a YALO_CONFIG.config_option -> 'a -> unit
end

module YALO_TYPES : sig

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
  type filepath = string list
  type fs

  type scan_kind =
    | Scan_disabled
    | Scan_forced
    | Scan_maybe

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
    (file:file -> linter:linter -> 'linter_input -> 'linter_output) -> unit

  type 'linter_input new_gen_unit_linter =
    ('linter_input,unit) new_gen_linter

  type ('a,'b) active_linters =
    ( linter * ('a, 'b) linter_function ) list

  type annot_desc =
    | Annot_begin_warning
    | Annot_end_warning
    | Annot_spec of string
    | Annot_check of string * bool (* true => the warning may be after *)

end

open YALO_TYPES

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
    ?set_by_default:bool ->
    name:string -> msg:string -> int -> warning
  val tag_danger : tag

  val warn : loc:location -> file:file -> linter:linter ->
    ?msg:string ->
    ?autofix:(YALO_TYPES.location * string) list ->
    warning -> unit

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
      'a YALO_CONFIG.option_class ->
      'a -> 'a YALO_CONFIG.config_option
  end

  module DOC_STORE : sig
    type 'a t
    val create : plugin -> 'a t
    val put : 'a t -> document -> 'a -> unit
    val check : 'a t -> document -> 'a option
    val get : 'a t -> document -> 'a
  end
  module FILE_STORE : sig
    type 'a t = 'a DOC_STORE.t
    val create : plugin -> 'a t
    val put : 'a t -> file -> 'a -> unit
    val check : 'a t -> file -> 'a option
    val get : 'a t -> file -> 'a
  end

  val string_of_loc : location -> string
  val eprintf :
    ?loc:YALO_TYPES.location -> ('a, out_channel, unit) format -> 'a
end


module YALO_FS : sig
  (*      val get_folder : fs -> string -> folder *)
  val folder : fs -> folder
end

module YALO_NS : sig
  val name : namespace -> string
end

module YALO_WARNING : sig
  val name : warning -> string
end

module YALO_FILE : sig
  val name : file -> string
end

module YALO_DOC : sig
  val name : document -> string
  val basename : document -> string
  val parent : document -> folder

  val set_other_name : document -> string -> unit
end

module YALO_FOLDER : sig
  val name : folder -> string
  val fs : folder -> fs
  val projects : folder -> project StringMap.t
  val folders : folder -> folder StringMap.t
  val parent : folder -> folder
  val basename : folder -> string
  val set_scan : folder -> scan_kind -> unit
  val set_projects : folder -> project StringMap.t -> unit
  val set_other_name : folder -> string -> unit
end

module YALO_LANG : sig

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
    ?on_open:(file:file -> linter:linter -> unit) ->
    ?on_close:(file:file -> linter:linter -> unit) ->
    ?on_end:(unit -> unit) ->
    (linter -> unit) -> unit

  val new_gen_linter :
    language ->
    (linter * 'a) list ref ->
    YALO_TYPES.namespace ->
    string ->
    warnings:YALO_TYPES.warning list ->
    ?on_begin:(unit -> unit) ->
    ?on_open:(file:YALO_TYPES.file -> linter:linter -> unit) ->
    ?on_close:(file:YALO_TYPES.file -> linter:linter -> unit) ->
    ?on_end:(unit -> unit) -> 'a -> unit

  val filter_linters :
    file:file -> ('a,'b) active_linters -> ('a, 'b) active_linters
  val lint_with_active_linters :
    ('a, unit) active_linters ref -> file:file -> 'a -> unit
  val iter_linters_open :
    file:YALO_TYPES.file -> ('a,'b) active_linters -> unit
  val iter_linters_close :
    file:YALO_TYPES.file -> ('a, 'b) active_linters -> unit
  val iter_linters :
    file:file -> ('a,unit) active_linters -> 'a -> unit

  val add_file_classifier :
    (file_doc:YALO_TYPES.document -> YALO_TYPES.file_kind option) ->
    unit
  val add_folder_updater : (folder:YALO_TYPES.folder -> unit) -> unit

  val add_annot :
    file:YALO_TYPES.file ->
    loc:YALO_TYPES.location -> YALO_TYPES.annot_desc -> unit

  val temp_set_option : string list -> string -> unit

  module Make_source_linters
      (M:sig
         val lang : language
       end) : sig

    val lint_src_file : file:YALO_TYPES.file -> unit

    val new_src_file_linter :
      namespace ->
      string ->
      warnings:warning list ->
      ?on_begin : (unit -> unit) ->
      ?on_end : (unit -> unit) ->
      (src_file_input, unit) linter_function -> unit
    val new_src_content_linter : src_content_input new_gen_unit_linter
    val new_src_line_linter : src_line_input new_gen_unit_linter
  end
  val set_lexbuf_filename : Lexing.lexbuf -> string -> unit
end

module YALO_INTERNAL : sig

  val doc_kind :
    file_doc:Types.document ->
    Types.file_kind option
  val add_file :
    file_doc:Types.document ->
    file_kind:Types.file_kind ->
    unit
  val new_file : (* used by ppx *)
    file_doc:Types.document ->
    file_kind:file_kind ->
    file_crc:Digest.t -> file
  val get_document : Types.folder -> string -> Types.document
end

val init : unit -> unit
