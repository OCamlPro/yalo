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

let filename_concat dir file =
  match dir with
  | "" | "." -> file
  | _ -> Printf.sprintf "%s/%s" dir file

let find_file ?from file =
  let rec iter dirname path =
    let filename = filename_concat dirname file in
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
          let fullname = filename_concat dir name in
          if Sys.file_exists fullname then fullname
          else try_dir rem
    in
    try_dir path

(* We could use Fpath.normalize, but Fpath uses backslashes while we
   have decided to only use slashes, even on Windows. *)
let path_of_filename ?(subpath=[]) filename =
  let rec remove_dotdot = function
    | ".." :: path -> remove_dotdot path
    | path -> path
  in
  let b = Bytes.of_string filename in
  for i = 0 to Bytes.length b -1 do
    if filename.[i] = '\\' then Bytes.set b i '/'
  done;
  let path = String.split_on_char '/' (Bytes.unsafe_to_string b) in
  let volume, path =
    match path with
    | [] -> None, []
    | partition :: right_path ->
        if String.contains partition ':' then
          let partition, left_path = EzString.cut_at partition ':' in
          Some ( partition ^ ":" ), left_path :: right_path
        else
          None, path
  in
  let is_absolute, path =
    match path with
    | "" :: (( _ :: _) as path) -> true, path
    | _ -> false, path
  in
  let path =
    match subpath with
    | [] -> path
    | _ ->
        if is_absolute || volume <> None then
          path
        else
          subpath @ path
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
  let path = normalize_path path in
  (if is_absolute then
     let path = remove_dotdot path in
     match volume, path with
     | None, [] -> [ "" ; "" ]
     | Some volume, [] -> [ volume ; "" ]
     | None, _ -> "" :: path
     | Some volume, _ -> volume :: path
   else
     match volume, path with
     | None, _ -> path
     | Some volume, [] -> [ volume ]
     | Some volume, x :: path -> (volume ^ x) :: path
  )


let filename_of_path path =
  match path with
  | [ ]  -> "."
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
          Printf.ksprintf failwith "Utils.safe_mkdir: %s is a special file" dir

let () =
  let list = [
    "", "." ;
    "/", "/" ;
    "/a", "/a" ;
    "/a/", "/a" ;
    "/a/.", "/a" ;
    "/a/b", "/a/b" ;
    "/a/b/", "/a/b" ;
    "/a/b/.", "/a/b" ;
    "/a/./b", "/a/b" ;
    "/./a/b", "/a/b" ;
    "/a/b/c/", "/a/b/c" ;
    "/a/b/../c/", "/a/c" ;
    "/../a/b/c/", "/a/b/c" ;
    "/../../a/b/c/", "/a/b/c" ;
    "./a/b/c/", "a/b/c" ;
    "./../a/b/c/", "../a/b/c" ;
    "../a/b/c/", "../a/b/c" ;
    "../../a/b/c/", "../../a/b/c" ;

    ".",  "." ;
    "..",  ".." ;
    "../",  ".." ;
    "../../",  "../.." ;
    "/",  "/" ;
    "/a/b/",  "/a/b" ;
    "/a/b",  "/a/b" ;
    "a/",  "a" ;
    "a",  "a" ;
    "a/.",  "a" ;
    "a/./",  "a" ;
    "a/..",  "." ;
    "a/../",  "." ;
    "a/..b",  "a/..b" ;
    "./a",  "a" ;
    "../a",  "../a" ;
    "../../a",  "../../a" ;
    "./a/..",  "." ;
    "/a/b/./..",  "/a" ;
    "/../..",  "/" ;
    "/a/../..",  "/" ;
    "./../..",  "../.." ;
    "../../a/",  "../../a" ;
    "/a/b/c/./../../g",  "/a/g" ;
    "/a/b/c/./../../g/",  "/a/g" ;
  ]
  in
  List.iter (fun (s1, s2) ->
      let s3 = normalize_filename s1  in
      if s3 <> s2 then begin
        Printf.eprintf "Developer error:\n%!";
        Printf.eprintf "normalize_filename(%S) = %S\n%!" s1 s3;
        Printf.eprintf "         but should be = %S\n%!" s2 ;
        assert false
      end;
    )
    (* NOT YET SUPPORTED
       "\\\\?\\UNC\\server\\share\\..",  "\\\\?\\UNC\\server\\share\\" ;
       "\\\\server\\share\\",  "\\\\server\\share\\" ;
    *)
    (
      list
      @ List.map (fun (s1, s2) ->
          "c:" ^ s1,
          if s2 = "." then "c:"
          else "c:" ^ s2) list
    )
