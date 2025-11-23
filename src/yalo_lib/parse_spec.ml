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
   | -NS (* disactivate all warnings of this plugin and
   use it for next warnings *)

   W_SPEC =
   | +tag (* activate all warnings with this tag in the plugin *)
   | -tag (* disactivate all warnings with this tag in the plugin *)
   | +num (* activate the warning with this number in the plugin *)
   | -num (* disactivate the warning with this number in the plugin *)
*)

open EzCompat
open Types
open Yalo_misc.Infix

exception SpecError

let unexpected_char spec i =
  Printf.eprintf
    "Configuration error: unexpected char '%c' at position %d in spec %S\n%!"
    spec.[i] i spec;
  raise SpecError

let unexpected_end spec i =
  Printf.eprintf
    "Configuration error: unexpected end of specification at position %d \
     in spec %S\n%!"
    i spec;
  raise SpecError

let parse_spec ~spec (set_function : warning_state -> warning -> unit) =
  let ignore_set = !!Config.ignore_namespaces in
  let get_tag tag_name =
    try
      Some ( Hashtbl.find GState.all_tags tag_name )
    with Not_found ->
      if not @@ StringSet.mem tag_name ignore_set then
        Printf.eprintf "Configuration error: unknown tag %S in %S\n%!"
          tag_name spec;
      None
  in
  let get_ns ns_name =
    try
      Some ( Hashtbl.find GState.all_namespaces ns_name )
    with Not_found ->
      if not @@ StringSet.mem ns_name ignore_set then
        Printf.eprintf "Configuration warning: unknown namespace %S in \
                        spec %S\n%!" ns_name spec;
      None
  in
  let len = String.length spec in
  let rec iter0 i =
    (* Printf.eprintf "iter0 %d\n%!" i; *)
    if i < len then
      match spec.[i] with
      | ' ' -> iter0 (i+1)
      | '+' -> iter1 (i+1) ~set:Warning_enabled
      | '?' -> iter1 (i+1) ~set:Warning_sleeping
      | '-' -> iter1 (i+1) ~set:Warning_disabled
      | '#' -> iter_tag (i+1) (i+1)
      | 'A'..'Z' -> iter_ns i (i+1)
      | ',' -> iter0 (i+1)
      | _c -> unexpected_char spec i

  and iter1 ~set i =
    (* Printf.eprintf "iter1 %d\n%!" i; *)
    if i = len then
      List.iter (set_function set) !GState.all_warnings
    else
      match spec.[i] with
      | ' ' -> iter1 ~set (i+1)
      | ',' ->
          List.iter (set_function set) !GState.all_warnings;
          iter0 (i+1)
      | '#' -> iter_tag (i+1) (i+1) ~set
      | 'A'..'Z' -> iter_ns i (i+1) ~set
      | _c -> unexpected_char spec i

  and iter_tag ?set pos0 i =
    (* Printf.eprintf "iter_tag %d\n%!" i; *)
    if i = len then
      set_tag ?set pos0 i
    else
      match spec.[i] with
      | ',' ->
          set_tag ?set pos0 i;
          iter0 (i+1)
      | 'a'..'z' | '0'..'9' | '_' ->
          iter_tag ?set pos0 (i+1)
      | ' ' ->
          set_tag ?set pos0 i;
          iter0 (i+1)
      | _c -> unexpected_char spec i

  and iter_ns ?set pos0 i =
    (* Printf.eprintf "iter_ns %d\n%!" i; *)
    if i = len then
      set_ns ?set pos0 i
    else
      match spec.[i] with
      | 'A'..'Z' | '0'..'9' | '_' -> iter_ns ?set pos0 (i+1)
      | ' ' -> iter_namespace_space ?set ~pos0 ~pos1:i (i+1)
      | ',' ->
          set_ns ?set pos0 i;
          iter0 (i+1)
      | '+' ->
          let ns = set_in_ns ?set pos0 i in
          iter_ns1 ns ~set:Warning_enabled (i+1)
      | '?' ->
          let ns = set_in_ns ?set pos0 i in
          iter_ns1 ns ~set:Warning_sleeping (i+1)
      | '-' ->
          let ns = set_in_ns ?set pos0 i in
          iter_ns1 ns ~set:Warning_disabled (i+1)
      | _c -> unexpected_char spec i

  and iter_namespace_space ?set ~pos0 ~pos1 i =
    (* Printf.eprintf "iter_namespace_space %d\n%!" i; *)
    if i = len then
      set_ns ?set pos0 pos1
    else
      match spec.[i] with
      | ' ' -> iter_namespace_space ?set ~pos0 ~pos1 (i+1)
      | ',' ->
          set_ns ?set pos0 pos1;
          iter0 (i+1)
      | '+' ->
          let ns = set_in_ns ?set pos0 pos1 in
          iter_ns1 ns ~set:Warning_enabled (i+1)
      | '?' ->
          let ns = set_in_ns ?set pos0 pos1 in
          iter_ns1 ns ~set:Warning_sleeping (i+1)
      | '-' ->
          let ns = set_in_ns ?set pos0 pos1 in
          iter_ns1 ns ~set:Warning_disabled (i+1)
      | _c -> unexpected_char spec i

  and iter_ns0 ns i = (* NAMESPACE_SPEC W_SPEC . *)
    (* Printf.eprintf "iter_ns0 %d\n%!" i; *)
    if i < len then
      match spec.[i] with
      | ' ' -> iter_ns0 ns (i+1)
      | ',' -> iter0 (i+1)
      | '+' -> iter_ns1 ns ~set:Warning_enabled (i+1)
      | '?' -> iter_ns1 ns ~set:Warning_sleeping (i+1)
      | '-' -> iter_ns1 ns ~set:Warning_disabled (i+1)
      | _ -> unexpected_char spec i

  and iter_ns1 ns ~set i = (* NAMESPACE_SPEC {+/-} . *)
    (* Printf.eprintf "iter_ns1 %d\n%!" i; *)
    if i = len then
      unexpected_end spec i
    else
      match spec.[i] with
      | ',' -> unexpected_end spec i
      | ' ' -> iter_ns1 ns ~set (i+1)
      | '#' -> iter_namespace_tag ns ~set ~pos0:(i+1) (i+1)
      | '0'..'9' -> iter_namespace_num ns ~set ~pos0:i (i+1)
      | _ -> unexpected_char spec i

  and iter_namespace_tag ns ~set ~pos0 i =
    (* Printf.eprintf "iter_namespace_tag %d\n%!" i; *)
    if i = len then
      set_namespace_tag ns ~set ~pos0 i
    else
      match spec.[i] with
      | ' ' ->
          set_namespace_tag ns ~set ~pos0 i;
          iter_ns0 ns (i+1)
      | ',' ->
          set_namespace_tag ns ~set ~pos0 i;
          iter0 (i+1)
      | 'a'..'z' | '0'..'9' | '_' -> iter_namespace_tag ns ~set ~pos0 (i+1)
      | '#' ->
          set_namespace_tag ns ~set ~pos0 i;
          iter_namespace_tag ns ~set ~pos0:(i+1) (i+1)
      | '+' ->
          set_namespace_tag ns ~set ~pos0 i;
          iter_ns1 ns ~set:Warning_enabled (i+1)
      | '?' ->
          set_namespace_tag ns ~set ~pos0 i;
          iter_ns1 ns ~set:Warning_sleeping (i+1)
      | '-' ->
          set_namespace_tag ns ~set ~pos0 i;
          iter_ns1 ns ~set:Warning_disabled (i+1)
      | _c -> unexpected_char spec i

  and iter_namespace_num ns ~set ~pos0 i =
    (* Printf.eprintf "iter_namespace_num %d\n%!" i; *)
    if i = len then
      set_namespace_num ns ~set ~pos0 i
    else
      match spec.[i] with
      | ' ' ->
          set_namespace_num ns ~set ~pos0 i;
          iter_ns0 ns (i+1)
      | ',' ->
          set_namespace_num ns ~set ~pos0 i;
          iter0 (i+1)
      | '0'..'9' -> iter_namespace_num ns ~set ~pos0 (i+1)
      | '+' ->
          set_namespace_num ns ~set ~pos0 i;
          iter_ns1 ns ~set:Warning_enabled (i+1)
      | '?' ->
          set_namespace_num ns ~set ~pos0 i;
          iter_ns1 ns ~set:Warning_sleeping (i+1)
      | '-' ->
          set_namespace_num ns ~set ~pos0 i;
          iter_ns1 ns ~set:Warning_disabled (i+1)
      | _c -> unexpected_char spec i

  and set_namespace_tag nso ~set ~pos0 i =
    match nso with
    | None -> ()
    | Some ns ->
        let tag_name = String.sub spec pos0 (i-pos0) in
        let nstag = Printf.sprintf "%s:%s" ns.ns_name tag_name in
        match Hashtbl.find GState.all_nstags nstag with
        | exception Not_found ->
            Printf.eprintf
              "Configuration warning: tag %S in spec %S does not appear in \
               namespace %s\n%!"
              tag_name spec ns.ns_name
        | r ->
            List.iter (set_function set) !r

  and set_namespace_num nso ~set ~pos0 i =
    match nso with
    | None -> ()
    | Some ns ->
        let num = String.sub spec pos0 (i-pos0) in
        match int_of_string num with
        | exception exn ->
            Printf.eprintf
              "Configuration warning: int_of_string(%S) raised %s\n%!"
              num (Printexc.to_string exn)
        | num ->
            match IntMap.find num ns.ns_warnings_by_num with
            | exception Not_found ->
                Printf.eprintf
                  "Configuration error: warning %d of spec %S does not \
                   appear in namespace %s\n%!"
                  num spec ns.ns_name
            | w -> set_function set w

  and set_function_default w =
    let set =
      if w.w_set_by_default then
        Warning_enabled
      else
        Warning_sleeping
    in
    set_function set w

  and set_tag ?set pos i =
    let tag_name = String.sub spec pos (i-pos) in
    match get_tag tag_name with
    | None -> ()
    | Some tag ->
        match set with
        | Some set ->
            List.iter (set_function set) tag.tag_warnings
        | None ->
            List.iter set_function_default tag.tag_warnings

  and set_ns ?set pos i =
    let ns_name = String.sub spec pos (i-pos) in
    match get_ns ns_name with
    | None -> ()
    | Some ns ->
        match set with
        | None ->
            IntMap.iter (fun _ w ->
                set_function_default w) ns.ns_warnings_by_num
        | Some set ->
            IntMap.iter (fun _ w ->
                set_function set w) ns.ns_warnings_by_num

  and set_in_ns ?set pos i =
    let ns_name = String.sub spec pos (i-pos) in
    let nso = get_ns ns_name in
    begin
      match nso, set with
      | Some ns, Some set ->
          IntMap.iter (fun _ w -> set_function set w) ns.ns_warnings_by_num
      | _ -> ()
    end;
    nso
  in
  iter0 0

let parse_spec_list list set_function =
  List.iter (fun spec ->
      parse_spec ~spec set_function
    ) list

