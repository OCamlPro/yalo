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

module OP = struct

  let filename_concat dir file =
    match dir with
    | "" | "." -> file
    | _ -> Printf.sprintf "%s/%s" dir file

  let (///) = filename_concat

end

open OP

let find_file ?from file =
  let rec iter dirname path =
    let filename = dirname /// file in
    if Sys.file_exists filename then
      filename, path
    else
      let newdir = Filename.dirname dirname in
      if newdir = dirname then
        raise Not_found
      else
        iter newdir (Filename.basename dirname :: path)
  in
  let from = match from with
    | None -> Sys.getcwd ()
    | Some dir -> dir
  in
  iter from []


let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else
    let rec try_dir = function
        [] -> raise Not_found
      | dir::rem ->
          let fullname = dir /// name in
          if Sys.file_exists fullname then fullname
          else try_dir rem
    in
    try_dir path

let path_of_filename ?(subpath=[]) filename =
  let b = Bytes.of_string filename in
  for i = 0 to Bytes.length b -1 do
    if filename.[i] = '\\' then Bytes.set b i '/'
  done;
  let path = String.split_on_char '/' (Bytes.unsafe_to_string b) in
  let path =
    match subpath with
    | [] -> path
    | _ ->
       match path with
       | "" :: _ :: _ -> path (* absolute path, won't work on windows *)
       | _ -> subpath @ path
  in
  let rec normalize_path path =
    match path with
      [] -> []
    | dir :: tail ->
       let dir = dir :: normalize_path tail in
       match dir with
       | "" :: path -> path
       | "." :: path -> path
       | ".." :: _ -> dir
       | _ :: ".." :: path -> path
       | _ -> dir
  in
  normalize_path path

let filename_of_path path =
  match path with
  | [ ]  -> "."
  | [ "" ]  -> "."
  | _ -> String.concat "/" path

let normalize_filename ?subpath filename =
  path_of_filename ?subpath filename |> filename_of_path

let rec safe_mkdir dir =
  match Unix.stat dir with
  | exception _ ->
     let dirdir = Filename.dirname dir in
     safe_mkdir dirdir;
     Unix.mkdir dir 0o755
  | st ->
     match st.st_kind with
     | S_DIR -> ()
     | _ ->
        Printf.kprintf failwith "Utils.safe_mkdir: %s is a special file" dir
