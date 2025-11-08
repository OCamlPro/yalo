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

(*
open EzCompat
open Ez_file.V1
open EzFile.OP
open Types

let version = 0

type cached_file = {
    cached_filename : string ;
    cached_crc : string ;
    mutable cached_warnings : cached_warnings StringMap.t ; (* w_idstr *)
  }

and cached_plugin = {
    cached_plugname : string ;
    cached_version : string ;
  }

and cached_warnings = {
    cached_plugin : cached_plugin ;
    cached_wnum : int ;
    cached_messages : (Types.location * string) list ;
  }

and cache = {
    mutable cached_files : cached_file StringMap.t ; (* by file name *)
  }

let cache_file = Constant.temp_dir /// Constant.cache_basename

let save cache =
  if not (Sys.file_exists Constant.temp_dir) then
    Unix.mkdir Constant.temp_dir 0o755 ;
  let oc = open_out_bin cache_file in
  output_binary_int oc version ;
  output_value oc cache ;
  close_out oc

let default_cache = {
    cached_files = StringMap.empty ;
  }

let load () =
  if Sys.file_exists cache_file then
    try
      let ic = open_in_bin cache_file in
      let cache_version = try input_binary_int ic with exn ->
                            close_in ic; raise exn
      in
      if cache_version <> version then begin
      Printf.eprintf
      "Configuration warning: wrong cache version %d instead of %d.\n%!
      " cache_version version ;
          raise Exit
        end ;
      let cache = try (input_value ic : cache) with exn ->
                    close_in ic;
                    raise exn in
      close_in ic;
      cache
    with
      Exit -> default_cache
    | exn ->
    Printf.eprintf
    "Configuration warning: reading cache raised exception %s.\n%!"
    (Printexc.to_string exn) ;
       default_cache
  else
    default_cache

(* Fill the file_warnings_done of all files in cache *)
let apply _cache = assert false (* TODO *)

let new_file file =
  {
    cached_filename = file.file_name ;
    cached_crc = file.file_crc ;
    cached_warnings = StringMap.empty ;
  }

let get_warning_messages ~file w =
  let map = ref [] in
  StringMap.iter (fun _ m ->
      if m.msg_warning == w then
        map := ( m.msg_loc , m.msg_string ) :: !map
    ) file.file_messages;
  !map

let update cache =

  let plugins = Hashtbl.create 13 in
  let get_cached_plugin plugin =
    try Hashtbl.find plugins plugin.plugin_name with
    | Not_found ->
       let cached_plugin =
         {
           cached_plugname = plugin.plugin_name ;
           cached_version = plugin.plugin_version ;
         }
       in
       Hashtbl.add plugins plugin.plugin_name cached_plugin;
       cached_plugin
  in
  Hashtbl.iter (fun _ file ->
      let cached_file =
        match StringMap.find file.file_name cache.cached_files with
        | cached_file ->
           if cached_file.cached_crc <> file.file_crc then
             new_file file
           else
             cached_file
        | exception Not_found -> new_file file
      in
      StringMap.iter (fun _ w ->
          try
            let cached_w = StringMap.find w.w_idstr
                             cached_file.cached_warnings in
            if w.w_plugin.plugin_version <>
                 cached_w.cached_plugin.cached_version then
              raise Not_found ;
            (* warning already cached. nothing to do *)
          with Not_found ->
            let cached_w = {
                cached_plugin = get_cached_plugin w.w_plugin ;
                cached_wnum = w.w_num ;
                cached_messages = get_warning_messages ~file w ;
              }
            in
            cached_file.cached_warnings <-
              StringMap.add w.w_idstr cached_w
                cached_file.cached_warnings
        ) !Engine.active_warnings

    ) Engine.all_files

 *)
