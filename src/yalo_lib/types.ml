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

type filepath = string list

type plugin = {
    plugin_name : string;
    plugin_version : string ;
    mutable plugin_languages : language StringMap.t ;
    mutable plugin_args :
              (string list * Ezcmd.V2.EZCMD.spec *
                 Ezcmd.V2.EZCMD.TYPES.info) list ;
  }

and language = {
    lang_plugin : plugin ;
    lang_name : string ;
    mutable lang_kinds : file_kind StringMap.t ;
  }

and file_kind = {
    kind_uid : int ; (* first field for polymorphic compare *)

    kind_language : language ;
    kind_name : string ;
    kind_exts : string list ;

    kind_validate : (file_doc:document -> bool) ;
    kind_lint : (file:file -> unit) ;
  }

and namespace = {
    ns_plugin : plugin ;
    ns_name : string;
    mutable ns_warnings_by_num : warning IntMap.t ;
    mutable ns_warnings_by_name : warning StringMap.t ;
    mutable ns_linters : linter StringMap.t ;
  }

and tag = {
    tag_name : string;
    mutable tag_warnings : warning list ;
  }

and warning = {
    w_namespace : namespace ;
    w_num : int ; (* uniq in namespace *)
    w_idstr : string ;
    w_name : string ;
    mutable w_tags : tag list ;
    mutable w_linters : linter StringMap.t ;
    w_msg : string ;

    (* If a namespace or tag is activated without +, is this warning
       set by default ? *)
    mutable w_set_by_default : bool ;
    mutable w_state : warning_state ;

    (* If the warning is triggered, is it an error ? *)
    mutable w_level_error : bool ;
    w_desc : string ;
  }

and warning_state =
  | Warning_disabled
  | Warning_sleeping
  | Warning_enabled

and project = {
    project_name : string ;
    mutable project_files : file list ;
  }

and fs = {
    fs_root : string ;
    fs_folder : folder ;
    (* the path to the sub-folder where we started before chdir to .yaloconf  *)
    fs_subpath : string list ;
    fs_project : project ;
  }

and folder = {
    folder_fs : fs ;
    folder_parent : folder ;

    folder_basename : string ;
    folder_name : string ; (* name till fs_root *)

    mutable folder_tags : StringSet.t ;
    mutable folder_projects : project StringMap.t ;
    mutable folder_scan : scan_kind ;
    mutable folder_docs : document StringMap.t ;
    mutable folder_folders : folder StringMap.t ;
  }

and scan_kind =
  | Scan_disabled
  | Scan_forced
  | Scan_maybe

and document = {
    doc_parent : folder ;

    doc_basename : string ;
    doc_name : string ; (* name till fs_root *)
    mutable doc_tags : StringSet.t ;
    mutable doc_file : file option ;
  }

and file = {
    file_kind : file_kind ; (* first field for polymorphic compare *)
    file_uid : int ;

    file_doc : document ;
    file_name : string ; (* same as file_doc.doc_name ! *)

    file_crc : Digest.t ;
    mutable file_warnings_done : StringSet.t ;
    mutable file_done : bool ;
    mutable file_projects : project StringMap.t ;

    (* messages that have been triggered by linting this file, they
       may actually target another file *)
    mutable file_messages : message StringMap.t ;
  }

and position = Lexing.position = {
      pos_fname : string;
      pos_lnum : int;
      pos_bol : int;
      pos_cnum : int;
    }

and location = Location.t = {
      loc_start: position;
      loc_end: position;
      loc_ghost: bool;
    }

and message = {
    msg_warning : warning ;
    msg_file : file ; (* Warning: this file may not be the file where
                         the error was spotted, for example a .cmt
                         file instead of the .ml in the location. *)
    msg_linter: linter ;
    msg_idstr : string ;
    msg_loc : location ;
    msg_string : string ;
    msg_autofix : ( location * string ) list ;
  }

and linter = {
    linter_name : string ;
    linter_idstr : string ;
    linter_lang : language ;
    linter_namespace : namespace ;
    (* TODO: check that all these warnings are defined in the same
       namespace *)
    linter_warnings : warning StringMap.t ;
    mutable linter_active : bool ;
    linter_begin : (unit -> unit);
    linter_open : (file:file -> linter:linter -> unit) ;
    linter_install : (linter -> unit) ;
    linter_close : (file:file -> linter:linter -> unit) ;
    linter_end : (unit -> unit);
  }

type yalo_project_file = {
    pr_filename : string ; (* name of the file with this content *)

    (* {project: PROJECT} files in this directory and sub-directories
       should be associated with project PROJECT *)
    mutable pr_project : string option;
    (* {anchor: BASENAME} files with this basename BASENAME should
       associate their directory with the current project *)
    mutable pr_anchors : StringSet.t ;
    (* {ignore: BASENAME} skip files with this BASENAME in the
       current directory *)
    mutable pr_ignores : StringSet.t ;
    (* {skipdir: true} skip all files in this directory and
       sub-directories *)
    mutable pr_skipdir : bool ;
    (* {obj: BASENAME} recognize all files with basename BASENAME
       as an object file for this project *)
    mutable pr_objs : StringSet.t ;
    (* {skipobj: BASENAME} skip object file with basename BASENAME.
       Used typically to get rid of object files generated by `dune`
       when they trigger warnings/errors *)
    mutable pr_skipobjs : StringSet.t ;
  }

type src_line_input = {
    line_loc : Location.t ;
    line_line : string ;
    line_sep : string ;
  }

type src_file_input = {
    file_loc : Location.t ;
  }

type src_content_input = {
    content_loc : Location.t ;
    content_string : string ;
  }

type file_attr =
  | Project of string list
  | Skipdir of bool
  | Tag of string

type message_format =
  | Format_Human
  | Format_Context
  | Format_Sarif
  | Format_Short

(* what the provided loc means for the warnings zone *)
type zone_mode =
  | Zone_begin
  | Zone_all

type zone = {
    zone_loc : location ;
    zone_rev_zone : zone option ;
    zone_target : target ;
    zone_spec : string ;
    zone_creator : file ;
    mutable zone_rev_changes : (warning * warning_state) list ;
  }

and target = {
    target_name : string ; (* normalized version *)
    target_uid : int ;
    mutable target_zones : zone list ;
    mutable target_checks : (string * location * bool (* after? *)) list ;
    mutable target_messages : message list ;
  }
