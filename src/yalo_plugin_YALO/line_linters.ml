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

open Yalo.V1
open Yalo_plugin_ocaml.V1
open YALO_INFIX

let is_unix = Sys.os_type = "Unix"

let tag_line = YALO.new_tag "line"
let tag_autofix = YALO.new_tag "autofix"

type w_ids = {
  w_id_line_too_long : int ;
  w_id_spaces_at_end : int ;
  w_id_tab_used : int ;
  w_id_non_printable_char : int ;
  w_id_no_final_newline : int ;
  w_id_windows_newline : int ;
}

let register ns section w_ids =

  let ns_name = YALO_NS.name ns in
  let max_line_length =
    YALO.CONFIG.create_option section
      ~path:[ ns_name; "max_line_length"]
      ~short_help:"Maximal length of line before warning YALO+1"
      YALO_CONFIG.int_option
      80
  in

  let w_line_too_long =
    YALO.new_warning ns
      ~name:"line_too_long" w_ids.w_id_line_too_long
      ~tags:[ tag_line ]
      ~msg:"Line too long (not more than 80 characters)"
  in
  let w_spaces_at_end =
    YALO.new_warning ns
      ~name:"spaces_at_end" w_ids.w_id_spaces_at_end
      ~tags:[ tag_line ; tag_autofix ]
      ~msg:"Line ends with spaces"
  in
  let w_tab_used =
    YALO.new_warning ns
      ~name:"tab_used" w_ids.w_id_tab_used
      ~tags:[ tag_line ]
      ~msg:"Line contains tabulations"
  in
  let w_non_printable_char =
    YALO.new_warning ns
      ~name:"non_printable_char" w_ids.w_id_non_printable_char
      ~tags:[ tag_line ]
      ~msg:"Line contains non-printable chars"
  in
  let w_no_final_newline =
    YALO.new_warning ns
      ~name:"no_final_newline" w_ids.w_id_no_final_newline
      ~tags:[ tag_line ; tag_autofix ]
      ~msg:"File does not end with a newline"
  in
  (* Disabled under Windows *)
  let w_windows_newline =
    YALO.new_warning ns
      ~name:"windows_newline" w_ids.w_id_windows_newline
      ~tags:[ tag_line ]
      ~msg:"Line contains a \\r\\n instead of only \\n"
  in

  OCAML_LANG.new_src_line_linter ns "check_line"
    ~warnings:[w_line_too_long;
               w_spaces_at_end ;
               w_tab_used ;
               w_non_printable_char ;
               w_no_final_newline ;
               w_windows_newline ;
              ]
    (fun ~file ~linter { line_loc = loc ;
                         line_line = line ;
                         line_sep = sep } ->

      begin
        let len = String.length sep in
        if is_unix && len > 1 && sep.[0] = '\r' then begin
          let loc = { loc with loc_start = loc.loc_end } in
          YALO.warn ~loc ~file ~linter w_windows_newline;
        end;
        if len = 0 && String.length line > 0  then begin
          let loc = { loc with loc_start = loc.loc_end } in
          YALO.warn ~loc ~file ~linter
            w_no_final_newline ~autofix:[loc,"\n"]
        end
      end;

      let len = String.length line in
      if len > 0 then begin
        if len > !!max_line_length
           &&
           not @@ OCAML_LANG.is_menhir_generated_file()
        then
          YALO.warn ~loc ~file ~linter w_line_too_long
            ~msg:(Printf.sprintf
                    "Line too long (not more than %d characters)"
                    !!max_line_length)
        ;
        if line.[len-1] = ' ' then begin
          let rec iter pos =
            if pos>0 &&
               match line.[pos-1] with
               | ' ' | '\t' -> true
               | _ -> false then
              iter (pos-1)
            else
              { loc with loc_start =
                           { loc.loc_start with
                             pos_cnum = loc.loc_start.pos_cnum
                                        + pos }}
          in
          let loc = iter (len-1) in
          YALO.warn ~loc ~file ~linter
            w_spaces_at_end ~autofix:[loc,""];
        end;
        let has_tab = ref false in
        let has_nonprintable = ref false in
        for i = 0 to len-1 do
          match line.[i] with
          | '\t' -> has_tab := true
          | c ->
              let cc = Char.code c in
              if cc < 32 || cc > 126 then
                has_nonprintable := true;
        done;
        if !has_tab then
          YALO.warn ~loc ~file ~linter w_tab_used;
        if !has_nonprintable then
          YALO.warn ~loc ~file ~linter w_non_printable_char
      end
    )
