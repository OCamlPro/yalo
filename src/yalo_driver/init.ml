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

open Yalo_misc.Ez_config.V1

let fsroot = ref None
let get_fs ?(needs_to_load_plugins=true) () =
  match !fsroot with
  | Some fs -> fs
  | None ->
      let can_load_plugins =
        needs_to_load_plugins
        && not !Args.arg_no_load_plugins
      in
      let fs =
        try
          Yalo.Main.init
            ?config_file: !Args.arg_config_file
            ~load_dirs: !Args.arg_load_dirs
            ~plugins: !Args.arg_load_plugins
            ~profiles: !Args.arg_profiles
            ~can_load_plugins
            ()
        with EZCONFIG.LoadError _ as exn ->
          Printf.eprintf "Configuration error: exception %s\n%!"
            (Printexc.to_string exn) ;
          exit 2
      in
      fsroot := Some fs;
      fs
