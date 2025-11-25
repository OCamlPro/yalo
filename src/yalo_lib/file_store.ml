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

let clear t doc =
  let uid = doc.doc_uid in
  let size = Array.length t.store in
  if size > uid then
    t.store.(uid) <- None

let put t doc x =
  let uid = doc.doc_uid in
  let size = Array.length t.store in
  if size <= uid then begin
    let old_store = t.store in
    let new_size =
      let rec iter size pos =
        if size <= pos then
          iter (2*size) pos
        else
          size
      in
      iter (2* size) uid
    in
    t.store <- Array.make new_size None;
    Array.blit old_store 0 t.store 0 size ;
  end;
  t.store.( uid ) <- Some x

let check t doc =
  let uid = doc.doc_uid in
  if Array.length t.store > uid then
    t.store.( uid )
  else
    None

exception No_data_in_store of string * string

let get t doc =
  match check t doc with
  | None ->
      raise @@ No_data_in_store (doc.doc_name, t.store_plugin.plugin_name)
  | Some x -> x

