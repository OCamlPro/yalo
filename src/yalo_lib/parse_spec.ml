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

(* a specification is a string:
   SPEC = SUBSPEC:SUBSPEC:SUBSPEC...
   SUBSPEC =
   | + (* activate all existing warnings *)
   | - (* disactivate all existing warnings *)
   | tag | +tag  (* activate all warnings with this tag *)
   | -tag        (* disactivate all warnings with this tag *)
   | NS_SPEC W_SPEC*

   NS_SPEC =
   | NS (* all following warnings are in this plugin. If no
    W_SPEC follows, then +NS is understood. *)
   | +NS (* activate all warnings of this plugin and use it for next warnings *)
   | -NS (* disactivate all warnings of this plugin and use it for next warnings *)

   W_SPEC =
   | +tag (* activate all warnings with this tag in the plugin *)
   | -tag (* disactivate all warnings with this tag in the plugin *)
   | +num (* activate the warning with this number in the plugin *)
   | -num (* disactivate the warning with this number in the plugin *)
 *)

open EzCompat
open Types

let unexpected_char spec i =
  Printf.eprintf "Configuration error: unexpected char '%c' at position %d in spec %S\n%!" spec.[i] i spec;
  exit 2

let unexpected_end spec i =
  Printf.eprintf "Configuration error: unexpected end of specification at position %d in spec %S\n%!" i spec;
  exit 2

let get_tag tag_name =
  try
    Hashtbl.find Engine.all_tags tag_name
  with Not_found ->
        Printf.eprintf "Configuration error: unknown tag %S\n%!" tag_name;
        exit 2

let get_ns plugin_name =
  try
    Hashtbl.find Engine.all_plugins plugin_name
  with Not_found ->
        Printf.eprintf "Configuration error: unknown plugin %S\n%!" plugin_name;
        exit 2

let parse_spec spec set_function =
  let len = String.length spec in
  let rec iter0 i =
    (* Printf.eprintf "iter0 %d\n%!" i; *)
    if i < len then
      match spec.[i] with
      | ' ' -> iter0 (i+1)
      | '+' -> iter1 (i+1) ~set:true
      | '-' -> iter1 (i+1) ~set:false
      | 'a'..'z' -> iter_tag ~set:true i (i+1)
      | 'A'..'Z' -> iter_ns i (i+1)
      | ':' -> iter0 (i+1)
      | _c -> unexpected_char spec i

  and iter1 ~set i =
    (* Printf.eprintf "iter1 %d\n%!" i; *)
    if i = len then
      List.iter (set_function set) !Engine.all_warnings
    else
      match spec.[i] with
      | ' ' -> iter1 ~set (i+1)
      | ':' ->
         List.iter (set_function set) !Engine.all_warnings;
         iter0 (i+1)
      | 'a'..'z' -> iter_tag i (i+1) ~set
      | 'A'..'Z' -> iter_ns i (i+1) ~set
      | _c -> unexpected_char spec i

  and iter_tag ~set pos0 i =
    (* Printf.eprintf "iter_tag %d\n%!" i; *)
    if i = len then
      set_tag ~set pos0 i
    else
      match spec.[i] with
      | ':' ->
         set_tag ~set pos0 i;
         iter0 (i+1)
      | 'a'..'z' | '0'..'9' | '_' -> iter_tag ~set pos0 (i+1)
      | ' ' ->
         set_tag ~set pos0 i;
         iter0 (i+1)
      | _c -> unexpected_char spec i

  and iter_ns ?set pos0 i =
    (* Printf.eprintf "iter_ns %d\n%!" i; *)
    if i = len then
      set_ns ?set pos0 i
    else
      match spec.[i] with
      | 'A'..'Z' | '0'..'9' | '_' -> iter_ns ?set pos0 (i+1)
      | ' ' -> iter_plugin_space ?set pos0 i (i+1)
      | ':' ->
         set_ns ?set pos0 i;
         iter0 (i+1)
      | '+' ->
         let ns = set_in_ns ?set pos0 i in
         iter_ns1 ns ~set:true (i+1)
      | '-' ->
         let ns = set_in_ns ?set pos0 i in
         iter_ns1 ns ~set:false (i+1)
      | _c -> unexpected_char spec i

  and iter_plugin_space ?set pos0 pos1 i =
    (* Printf.eprintf "iter_plugin_space %d\n%!" i; *)
    if i = len then
      set_ns ?set pos0 pos1
    else
      match spec.[i] with
      | ' ' -> iter_plugin_space ?set pos0 pos1 (i+1)
      | ':' ->
         set_ns ?set pos0 pos1;
         iter0 (i+1)
      | '+' ->
         let ns = set_in_ns ?set pos0 pos1 in
         iter_ns1 ns ~set:true (i+1)
      | '-' ->
         let ns = set_in_ns ?set pos0 pos1 in
         iter_ns1 ns ~set:false (i+1)
      | _c -> unexpected_char spec i

  and iter_ns0 ns i = (* PLUGIN_SPEC W_SPEC . *)
    (* Printf.eprintf "iter_ns0 %d\n%!" i; *)
    if i < len then
      match spec.[i] with
      | ' ' -> iter_ns0 ns (i+1)
      | ':' -> iter0 (i+1)
      | '+' -> iter_ns1 ns ~set:true (i+1)
      | '-' -> iter_ns1 ns ~set:false (i+1)
      | _ -> unexpected_char spec i

  and iter_ns1 ns ~set i = (* PLUGIN_SPEC {+/-} . *)
    (* Printf.eprintf "iter_ns1 %d\n%!" i; *)
    if i = len then
      unexpected_end spec i
    else
      match spec.[i] with
      | ':' -> unexpected_end spec i
      | ' ' -> iter_ns1 ns ~set (i+1)
      | 'a'..'z' -> iter_plugin_tag ns ~set i (i+1)
      | '0'..'9' -> iter_plugin_num ns ~set i (i+1)
      | _ -> unexpected_char spec i

  and iter_plugin_tag ns ~set pos0 i =
    (* Printf.eprintf "iter_plugin_tag %d\n%!" i; *)
    if i = len then
      set_plugin_tag ns ~set pos0 i
    else
      match spec.[i] with
      | ' ' ->
         set_plugin_tag ns ~set pos0 i;
         iter_ns0 ns (i+1)
      | ':' ->
         set_plugin_tag ns ~set pos0 i;
         iter0 (i+1)
      | 'a'..'z' | '0'..'9' | '_' -> iter_plugin_tag ns ~set pos0 (i+1)
      | '+' ->
         set_plugin_tag ns ~set pos0 i;
         iter_ns1 ns ~set:true (i+1)
      | '-' ->
         set_plugin_tag ns ~set pos0 i;
         iter_ns1 ns ~set:false (i+1)
      | _c -> unexpected_char spec i

  and iter_plugin_num ns ~set pos0 i =
    (* Printf.eprintf "iter_plugin_num %d\n%!" i; *)
    if i = len then
      set_plugin_num ns ~set pos0 i
    else
      match spec.[i] with
      | ' ' ->
         set_plugin_num ns ~set pos0 i;
         iter_ns0 ns (i+1)
      | ':' ->
         set_plugin_num ns ~set pos0 i;
         iter0 (i+1)
      | '0'..'9' -> iter_plugin_num ns ~set pos0 (i+1)
      | '+' ->
         set_plugin_num ns ~set pos0 i;
         iter_ns1 ns ~set:true (i+1)
      | '-' ->
         set_plugin_num ns ~set pos0 i;
         iter_ns1 ns ~set:false (i+1)
      | _c -> unexpected_char spec i

  and set_plugin_tag ns ~set pos i =
    let tag_name = String.sub spec pos (i-pos) in
    let nstag = Printf.sprintf "%s:%s" ns.plugin_name tag_name in
    match Hashtbl.find Engine.all_nstags nstag with
    | exception Not_found ->
       Printf.eprintf "Configuration error: tag %S does not appear in plugin %s\n%!" tag_name ns.plugin_name;
       exit 2
    | r ->
       List.iter (set_function set) !r

  and set_plugin_num ns ~set pos i =
    let num = String.sub spec pos (i-pos) in
    let num =
      try int_of_string num
      with exn ->
        Printf.eprintf
          "Configuration error: int_of_string(%S) raised %s\n%!"
          num (Printexc.to_string exn);
        exit 2

    in
    match IntMap.find num ns.plugin_warnings with
    | exception Not_found ->
       Printf.eprintf "Configuration error: warning %d does not appear in plugin %s\n%!" num ns.plugin_name;
       exit 2
    | w -> set_function set w

  and set_tag ~set pos i =
    let tag_name = String.sub spec pos (i-pos) in
    let tag = get_tag tag_name in
    List.iter (set_function set) tag.tag_warnings

  and set_ns ?set pos i =
    let set = match set with
      | None -> true
      | Some set -> set
    in
    let _ns = set_in_ns ~set pos i in
    ()

  and set_in_ns ?set pos i =
    let plugin_name = String.sub spec pos (i-pos) in
    let ns = get_ns plugin_name in
    begin
      match set with
      | None -> ()
      | Some set ->
         IntMap.iter (fun _ w -> set_function set w) ns.plugin_warnings
    end;
    ns
  in
  iter0 0

let parse_spec_list list set_function =
  List.iter (fun spec ->
      parse_spec spec set_function
    ) list

