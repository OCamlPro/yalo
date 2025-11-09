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

(* We should provide a string based version of Ez_config.
   Load and save should work with an optionnal filename.
 *)


open Yalo_misc.Ez_config.V1
open Types

let config_file =
  EZCONFIG.create_config_file Constant.config_basename

let load filename =
  EZCONFIG.load config_file ~filename

let save filename =
  EZCONFIG.save_with_help config_file ~filename

let append filename =
  EZCONFIG.append config_file filename

let get_simple_option option =
  EZCONFIG.get_simple_option config_file option

let set_simple_option option v =
  EZCONFIG.set_simple_option config_file option v

let create_config_section plugin ~short_help =
  EZCONFIG.create_config_section config_file
    [ plugin.plugin_name ] short_help

let create_config_option name ~path =
  EZCONFIG.create_section_option name path

let main_section = EZCONFIG.create_config_section
                     config_file ["main"]
                     "Options for Yalo itself"

let config_load_plugins =
  create_config_option main_section
    ~path: [ "plugins" ]
    ~short_help:"List of plugins to load (in this specific order)"
    EZCONFIG.string_list_option
    [ "yalo_plugin_ocaml"; "yalo_plugin_YALO" ]

let config_load_dirs =
  create_config_option main_section
    ~path:[ "search_path" ]
    ~short_help:"List of directories to search for plugin/profile files"
    EZCONFIG.string_list_option
    [ ]

let config_warnings =
  create_config_option main_section
    ~path:[ "warnings" ]
    ~short_help:"List of specifications to activate warnings"
    EZCONFIG.string_list_option
    [ "YALO" ]

let config_errors =
  create_config_option main_section
    ~path:[ "errors" ]
    ~short_help:"List of specifications to activate errors"
    EZCONFIG.string_list_option
    [ "#danger" ]

module FILEATTR = struct
  open EZCONFIG

  let of_option = function
    | Module list ->
       List.map (function
           | "project", StringValue v ->
              Project ( EzString.split v ',' )
           | "project", v ->
              Project ( EZCONFIG.value_to_list EZCONFIG.value_to_string v )
           | "skipdir", v ->
              Skipdir ( EZCONFIG.value_to_bool v )
           | "tag", v ->
              Tag ( EZCONFIG.value_to_string v )
           | s, _ ->
              Printf.ksprintf failwith
                "Unknown fileattr name %S" s
         ) list
    | _ -> failwith "Parse error"

  let to_option fileattrs =
    Module (
    List.map (function
        | Project [ name ] ->
           "project", StringValue name ;
        | Project list ->
           "project",
           EZCONFIG.list_to_value
             EZCONFIG.string_to_value list
        | Tag tagname ->
           "tag",
           EZCONFIG.string_to_value tagname ;
        | Skipdir bool ->
           "skipdir",
           EZCONFIG.bool_to_value bool ;
      ) fileattrs
      )

  let option =
    EZCONFIG.define_option_class "fileattr"
      of_option to_option

end

let project =
  create_config_option main_section
    ~path:[ "project" ]
    ~short_help:"The name of this project, starting from the root"
    EZCONFIG.string_option
    "default"

let default_target =
  create_config_option main_section
    ~path:[ "default_target" ]
    ~short_help:"The name of the project to lint by default"
    (EZCONFIG.option_option EZCONFIG.string_option)
    None

let fileattrs =
  create_config_option main_section
    ~path:[ "fileattrs" ]
    ~short_help:"Associate attributes with files and folders"
    (EZCONFIG.list_option
       (EZCONFIG.tuple2_option
          (EZCONFIG.string_option, FILEATTR.option)))
    []

let config_profiles =
  create_config_option main_section
    ~path:[ "profiles" ]
    ~short_help:"List of profiles to use for this project (NAME means \
                 yalo-NAME.conf should be loaded)"
    EZCONFIG.string_list_option
    []






(* Modify main.ml to append these options for all profiles *)

let profiles_section =
  EZCONFIG.create_config_section
    config_file ["profiles"]
    "Options with special behavior in profiles "

let profile_plugins =
  create_config_option profiles_section
    ~path: [ "profile_plugins" ]
    ~level:10
    ~short_help:"List of plugins to load (in this specific order) \
                 for this profile"
    EZCONFIG.string_list_option
    []

let profile_load_dirs =
  create_config_option profiles_section
    ~path:[ "profile_search_path" ]
    ~level:10
    ~short_help:"List of directories to search for plugin files for \
                 this profile"
    EZCONFIG.string_list_option
    []

let profile_profiles =
  create_config_option profiles_section
    ~path:[ "profile_profiles" ]
    ~short_help:"List of other profiles to use for this profile"
    ~level:10
    EZCONFIG.string_list_option
    []

let profile_fileattrs =
  create_config_option profiles_section
    ~path:[ "profile_fileattrs" ]
    ~level:10
    ~short_help:"Associate attributes with files and folders"
    (EZCONFIG.list_option
       (EZCONFIG.tuple2_option
          (EZCONFIG.string_option, FILEATTR.option)))
    []

let profile_warnings =
  create_config_option profiles_section
    ~path:[ "profile_warnings" ]
    ~short_help:"List of specifications to activate warnings"
    ~long_help: [
      "List of specifications to activate warnings. These specifications";
      "are used before the ones provided by the configuration, so that";
      "the project configuration has the final word.";

    ]
    EZCONFIG.string_list_option
    []

let profile_errors =
  create_config_option profiles_section
    ~path:[ "profile_errors" ]
    ~short_help:"List of specifications to activate errors"
    EZCONFIG.string_list_option
    []
