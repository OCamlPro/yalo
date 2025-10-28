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


open Ez_file.V1
open Yalo_misc.Ez_json.TYPES

open Types



let show_context loc =
  let start = loc.loc_start in
  let stop = loc.loc_end in
  let file_name = start.pos_fname in
  try
    let lines = EzFile.read_lines file_name in
    let rec iter i lines =
      if i < Array.length lines then
        if i >= start.pos_lnum-3 then begin
            Printf.eprintf "%05d %c %s\n%!"
              (i+1)
              (if i+1 >= start.pos_lnum &&
                    i+1 <= stop.pos_lnum then
                '>'
              else ' ')
              lines.(i)
          ;
            if i+1 = stop.pos_lnum then begin
                let c1 = start.pos_cnum - start.pos_bol in
                let c2 = stop.pos_cnum - stop.pos_bol in
                let c0 = min c1 c2 in
                let c1 = max c1 c2 in
                Printf.eprintf "%s%s\n%!"
                  (String.make (c0+5+3) ' ')
                  (String.make (c1-c0) '^')

              end;
            if i < stop.pos_lnum+2 then
              iter (i+1) lines
          end
        else
          iter (i+1) lines
    in
    iter 0 lines
  with _ -> ()


let display_human ~format messages =
  match messages with
  | [] -> 0
  | messages ->
     let nwarnings = ref 0 in
     let nerrors = ref 0 in
     List.iter (fun m ->
         if m.msg_warning.w_level_error then
           incr nerrors
         else
           incr nwarnings ;
       ) messages ;
     begin
       match !nwarnings, !nerrors with
       | 0, n ->
          Printf.eprintf "Yalo: %d errors found\n" n
       | n, 0 ->
          Printf.eprintf "Yalo: %d warnings found\n" n
       | nw, ne ->
          Printf.eprintf "Yalo: %d errors and %d warnings found\n" ne nw
     end;
     List.iter (fun m ->
         match format with
         | Format_Human | Format_Context ->
            (* warning: src/main.rs:2:5: unnecessary repetition *)
            Printf.eprintf "%d-%d\n%!"
                m.msg_loc.loc_start.pos_cnum
                m.msg_loc.loc_end.pos_cnum;
            Location.print_loc Format.str_formatter m.msg_loc;
            let loc = Format.flush_str_formatter () in
            Printf.eprintf "%s\n%!" loc;
            Printf.eprintf "%s (%s): %s\n%!"
              (if m.msg_warning.w_level_error then
                "Error"
              else
                "Warning")
              m.msg_warning.w_idstr
              m.msg_string;
            begin
              match m.msg_autofix with
              | None -> ()
              | Some text ->
                 Printf.eprintf "  Possible replacement (--autofix): %S\n"
                   text
            end;
            if format = Format_Context then
              show_context m.msg_loc

         | Format_Short ->
            let pos = m.msg_loc.loc_start in
            Printf.eprintf "%s: %s:%d:%d: %s %s\n%!" 
            (if m.msg_warning.w_level_error then
              "error"
            else
              "warning")
              pos.pos_fname
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol + 1)
              m.msg_warning.w_idstr
              m.msg_string;
         | _ -> assert false
       ) messages ;
     !nerrors

(* TODO: we should use the 'sarif' package instead *)
let display_sarif messages =
  let nerrors = ref 0 in
  let results =
    List.map (fun m ->
        let loc = m.msg_loc in
        let start = loc.loc_start in
        let stop = loc.loc_end in
        
        let file_object =
          OBJECT [
              "uri", STRING start.pos_fname ;
            ]
        in

        let loc_object =
          OBJECT [
              "startLine", INT start.pos_lnum ;
              "startColumn", INT (start.pos_cnum - start.pos_bol + 1);
              "endLine", INT stop.pos_lnum ;
              "endColumn", INT (stop.pos_cnum - stop.pos_bol + 1); 
          ]
        in
        
        let fixes =
          if false then
            []
          else
            [
              OBJECT [
                  "artifactChanges",
                  LIST [
                      OBJECT [
                          "artifactLocation", file_object ;
                          "replacements",
                          LIST [
                              OBJECT [
                                  "deletedRegion", loc_object ;
                                  "insertedContent",
                                  OBJECT [
                                      "text", STRING "";
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]

        in
        let fields = [
            "ruleId", STRING m.msg_warning.w_idstr ;
            "level", STRING (if m.msg_warning.w_level_error then
                         "error"
                       else
                         "warning");
            "message", OBJECT [
                           "text", STRING m.msg_string ;
                         ];
            "locations",
            LIST [
                OBJECT [
                    "physicalLocation",
                    OBJECT [
                        "artifactLocation", file_object ;
                        "region", loc_object ;
                      ]
                  ]
              ]
          ]
        in
        let fields = match fixes with
          | [] -> fields
          | _ -> fields @ [ "fixes", LIST fixes ]
        in
        OBJECT fields
      ) messages
  in
  let str =
    Yalo_misc.Ez_json.to_string
      (OBJECT [
           "$schema", STRING "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.5.json" ;
           "version", STRING "2.1.0";
           "runs",
           LIST [
               OBJECT [
                   "tool",
                   OBJECT [
                       "driver",
                       OBJECT [
                           "name", STRING "yalo";
                           "version", STRING Version.version ;
                         ]
                     ];
                   "results", LIST results
                 ]
             ]
      ])
  in
  Printf.printf "%s%!\n" str ;
  !nerrors

let display_messages ?(format=Format_Human) messages =
  let nerrors =
    match format with
    | Format_Human
      | Format_Context -> display_human ~format messages
    | Format_Sarif -> display_sarif messages
    | Format_Short -> display_human ~format messages
  in
  if nerrors > 0 then exit 2
