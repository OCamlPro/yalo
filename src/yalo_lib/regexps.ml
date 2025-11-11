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

module MATCHER : sig

  type 'a t

  type kind =
    | Pcre
    | Glob
    | Emacs
    | Posix
    | Perl

  val create :
    ?exact:bool ->
    ?kind:kind ->
    (string * 'a) list -> 'a t

  val find_first_match : 'a t -> string -> (int * int * 'a) option
  val find_all : 'a t -> string -> (int * int * 'a list) option

end = struct

  module MARK_MAP = Map.Make ( Re.Mark )

  type 'a t = Re.re * 'a MARK_MAP.t

  type kind =
    | Pcre
    | Glob
    | Emacs
    | Posix
    | Perl

  let create_matcher ~compile list =
    let map = ref MARK_MAP.empty in
    let groups = ref [] in

    List.iter (fun (re, value) ->
        match
          compile re
        with
        | exception exn ->
            Printf.eprintf "Error with regexp %S\n%!" re;
            Printf.eprintf "Exception %s\n%!" (Printexc.to_string exn);
            ()
        | re ->
            let mark, group = Re.mark re in
            map := MARK_MAP.add mark value !map ;
            groups := group :: !groups
      ) list;

    let re = Re.alt (List.rev !groups) in
    (re, !map)

  let exact_match re =
    Re.seq [ Re.start ; re ; Re.stop ]

  let create ?(exact=false)  ?(kind = Glob) list =
    let compile = match kind with
      | Emacs -> fun s -> Re.Emacs.re s
      | Glob -> fun s -> Re.Glob.glob s
      | Pcre -> fun s -> Re.Pcre.re s
      | Posix -> fun s -> Re.Posix.re s
      | Perl -> fun s -> Re.Perl.re s
    in
    let (re, map) = create_matcher ~compile list in
    let re = Re.compile ( if exact then exact_match re else re) in
    (re, map)

  let find_first_match (re,map) str =
    match Re.exec re str with
    | exception Not_found -> None
    | group ->
        let marks = Re.Mark.all group in
        let min = Re.Mark.Set.min_elt marks in
        Some (Re.Group.start group 0,
              Re.Group.stop group 0,
              MARK_MAP.find min map)

  let find_all (re,map) str =
    match Re.exec re str with
    | exception Not_found -> None
    | group ->
        let list = ref [] in
        let marks = Re.Mark.all group in
        Re.Mark.Set.iter (fun mark ->
            list := MARK_MAP.find mark map :: !list
          ) marks ;
        Some (Re.Group.start group 0,
              Re.Group.stop group 0,
              List.rev !list)

end

(*
(* --- EXAMPLE USAGE --- *)

let my_rules =
  MATCHER.create
    ~kind:Pcre
    [
      ("foo", "Found 'foo'");
      ("\\d{4}", "Found four digits");
      ("^[A-Z]+$", "Found all caps");
      ("invalid_regex([", "This one is broken");
      (* This will be safely skipped *)
      ("bar$", "Found 'bar' at the end");
    ]

let test_string str =
  match MATCHER.find_first_match my_rules str with
  | Some (pos, endpos, value) ->
  Printf.printf "Input: \"%s\"  ->  Match: \"%s\" (%S)\n" str value
                                   (String.sub str pos (endpos - pos))
  | None -> Printf.printf "Input: \"%s\"  ->  No Match\n" str

let tests () =
  test_string "hello 1234 world";
  test_string "this is foo";
  test_string "WORLD";
  test_string "this ends with bar";
  test_string "just text";
  exit 2
 *)
