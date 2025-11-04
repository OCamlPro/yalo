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

open EzCompat (* for IntMap *)
open Types

let new_counter () =
  let r = ref 0 in
  fun () ->
  let v = !r in
  incr r;
  v

let new_file_kind_uid = new_counter ()
let new_file_uid = new_counter ()
let new_target_uid = new_counter ()

let verbosity = ref 0

let profiles_fileattrs = ref ([] : (string * Types.file_attr list) list)
let profiles_load_dirs = ref ([] : string list)
let profiles_plugins = ref ([] : string list)
let profiles_profiles = ref ([] : string list)
let profiles_warnings = ref ([] : string list)
let profiles_errors = ref ([] : string list)

let all_plugins = (Hashtbl.create 13 : (string, plugin) Hashtbl.t)
let all_languages = (Hashtbl.create 13 : (string, language) Hashtbl.t)
let all_namespaces = (Hashtbl.create 13 : (string, namespace) Hashtbl.t)
let all_extensions = (Hashtbl.create 13 : (string, file_kind) Hashtbl.t)
let all_file_kinds = (Hashtbl.create 13 : (int, file_kind) Hashtbl.t)
let all_tags = (Hashtbl.create 13 : (string, tag) Hashtbl.t)
let all_nstags = (Hashtbl.create 13 : (string, warning list ref) Hashtbl.t)
let all_targets = (Hashtbl.create 13 : (string, target) Hashtbl.t)

let message_targets = ref (IntMap.empty : target IntMap.t)

let all_warnings = ref ([] : warning list)
let all_linters = ref ([] : linter list)
let all_plugins_args =
  ref ([] :
         (string list * Ezcmd.V2.EZCMD.spec * Ezcmd.V2.EZCMD.TYPES.info)
           list)
let all_files = (Hashtbl.create 113 : (string, file) Hashtbl.t)
let all_projects = (Hashtbl.create 13 : (string, project) Hashtbl.t)

let active_warnings = ref (StringMap.empty : warning StringMap.t)
let active_linters = ref ([] : linter list)

let file_classifier =
  ref ( (fun ~file_doc:_ -> None) : file_doc:document -> file_kind option)
let folder_updater =
  ref ( (fun ~folder:(_:folder) -> ()) : folder:folder -> unit)

let messages = ref ( [] : message list )

let restore_after_file_lint = ref ( [] : (unit -> unit) list)
