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

    let create_section_option section path ~short_help
          ?(long_help = [ short_help ]) ?level option_type default_value =
      create_section_option section path ~short_help long_help
        ?level option_type default_value

    let string_list_option = list_option string_option

    module OP = EzConfig.OP

  end
end
