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

open Types

type 'a t = {
    mutable store : 'a option array ;
    store_plugin : plugin ;
  }

let create store_plugin =
  {
    store = Array.make 1000 None ;
    store_plugin ;
  }

let put t file x =
  let size = Array.length t.store in
  if size <= file.file_uid then begin
      let old_store = t.store in
      let new_size =
        let rec iter size pos =
          if size <= pos then
            iter (2*size) pos
          else
            size
        in
        iter (2* size) file.file_uid
      in
      t.store <- Array.make new_size None;
      Array.blit old_store 0 t.store 0 size ;
    end;
  t.store.( file.file_uid ) <- Some x

let check t file =
  if Array.length t.store > file.file_uid then
    t.store.( file.file_uid )
  else
    None

exception No_data_in_store of string * string

let get t file =
  match check t file with
  | None ->
     raise @@ No_data_in_store (file.file_name, t.store_plugin.plugin_name)
  | Some x -> x
  
