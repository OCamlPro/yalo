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

let plugin = YALO.new_plugin "yalo_plugin_YALO" ~version:"0.1.0"

let ns = YALO.new_namespace plugin "YALO"

let tag_line = YALO.new_tag "line"
let tag_autofix = YALO.new_tag "autofix"

let w_line_too_long =
  YALO.new_warning ns
    ~name:"line-too-long" 1
    ~tags:[ tag_line ]
    ~msg:"Line too long (not more than 80 characters)"

let w_spaces_at_end =
  YALO.new_warning ns
    ~name:"spaces-at-end" 2
    ~tags:[ tag_line ; tag_autofix ]
    ~msg:"Line ends with spaces"

let w_tab_used =
  YALO.new_warning ns
    ~name:"tab-used" 3
    ~tags:[ tag_line ]
    ~msg:"Line contains tabulations"

let w_non_printable_char =
  YALO.new_warning ns
    ~name:"non-printable-char" 4
    ~tags:[ tag_line ]
    ~msg:"Line contains non-printable chars"

let w_no_final_newline =
  YALO.new_warning ns
    ~name:"no-final-newline" 5
    ~tags:[ tag_line ; tag_autofix ]
    ~msg:"File does not end with a newline"

let w_windows_newline =
  YALO.new_warning ns
    ~name:"windows-newline" 6
    ~tags:[ tag_line ]
    ~msg:"Line contains a \\r\\n instead of only \\n"

let () =
  OCAMLLANG.new_src_line_linter ns "ocp_check_line"
    ~warnings:[w_line_too_long;
               w_spaces_at_end ;
               w_tab_used ;
               w_non_printable_char ;
               w_no_final_newline ;
               w_windows_newline ;
    ]
    (fun ~file { line_loc = loc ;
                line_line = line ;
                line_sep = sep } ->

      begin
        let len = String.length sep in
        if len > 1 && sep.[0] = '\r' then begin
            let loc = { loc with loc_start = loc.loc_end } in
            YALO.warn loc ~file w_windows_newline;
          end;
        if len = 0 && String.length line > 0  then begin
            let loc = { loc with loc_start = loc.loc_end } in
            YALO.warn loc ~file w_no_final_newline ~autofix:"\n"
          end
      end;

      let len = String.length line in
      if len > 0 then begin
          if len > 80 then
            YALO.warn loc ~file w_line_too_long;
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
              YALO.warn loc ~file w_spaces_at_end ~autofix:"";
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
            YALO.warn loc ~file w_tab_used;
          if !has_nonprintable then
              YALO.warn loc ~file w_non_printable_char
        end
    )
