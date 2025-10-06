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

module EzConfig = struct
  module V1 = struct

    module EZCONFIG = struct

      module FILE = Ez_file.FileAbstract
      include EzConfig

      let create_config_file filename =
        EzConfig.create_config_file (FILE.of_string filename)
      
      let load ?filename config_file =
        begin match filename with
        | None -> ()
        | Some filename ->
           EzConfig.set_config_file config_file (FILE.of_string filename);
        end;
        EzConfig.load config_file
      
      let append config_file filename =
        EzConfig.append config_file (FILE.of_string filename)
      
      let save ?filename config_file =
        begin match filename with
        | None -> ()
        | Some filename ->
           EzConfig.set_config_file config_file (FILE.of_string filename);
        end;
        EzConfig.save config_file

      let save_with_help ?filename config_file =
        begin match filename with
        | None -> ()
        | Some filename ->
           EzConfig.set_config_file config_file (FILE.of_string filename);
        end;
        EzConfig.save_with_help config_file

      include EzConfig.LowLevel

      let create_section_option section path ~short_help ?(long_help = [ short_help ]) ?level option_type default_value =
        LowLevel.create_section_option section path ~short_help long_help ?level option_type default_value

      let string_list_option = list_option string_option

      module OP = EzConfig.OP
    end
  end
end

(* open Types *)
open EzConfig.V1
module OP = EZCONFIG.OP

let config_file =
  EZCONFIG.create_config_file Constant.config_basename
let load filename =
  EZCONFIG.load config_file ~filename

let save filename =
  EZCONFIG.save_with_help config_file ~filename

let append filename =
  EZCONFIG.append config_file filename

let create_config_section name ~short_help =
  EZCONFIG.create_config_section config_file [ name ] short_help

let create_config_option section ~path = EZCONFIG.create_section_option section path

let main_section = create_config_section "main" ~short_help:"Options for Yalo itself"

let config_load_plugins =
  create_config_option main_section
    ~path: [ "load_plugins" ]
    ~short_help:"List of plugins to load"
    EZCONFIG.string_list_option
    [ "yalo_plugin_YALO" ]

let config_load_dirs =
  create_config_option main_section
    ~path:[ "load_dirs" ]
    ~short_help:"List of directories to search for plugin files"
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
    [ "danger" ]
