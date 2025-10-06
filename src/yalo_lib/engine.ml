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

open EzCompat (* for IntMap *)
open Types

let file_uids = ref 0

let all_plugins = Hashtbl.create 13
let all_tags = Hashtbl.create 13
let all_nstags = Hashtbl.create 13
let all_warnings = ref []
let all_linters = ref []

let all_files = (Hashtbl.create 113 : (string, file) Hashtbl.t)
let all_projects = (Hashtbl.create 13 : (string, project) Hashtbl.t)

let active_warnings = ref StringMap.empty
let active_linters = ref []
let active_src_line_linters =
  ref
    ([] : (linter * (file:file -> src_line_input -> unit)) list )
let active_src_file_linters =
  ref
    ([] : (linter * (file:file -> src_file_input -> unit)) list )
let active_src_content_linters =
  ref
    ([] : (linter * (file:file -> src_content_input -> unit)) list )

let active_ast_intf_linters =
  ref
    ([] : (linter * (file:file -> Ppxlib.Parsetree.signature -> unit)) list )

let active_ast_impl_linters =
  ref
    ([] : (linter * (file:file -> Ppxlib.Parsetree.structure -> unit)) list )

let active_tast_intf_linters =
  ref
    ([] : (linter * (file:file -> Typedtree.signature -> unit)) list )

let active_tast_impl_linters =
  ref
    ([] : (linter * (file:file -> Typedtree.structure -> unit)) list )

let active_sig_linters =
  ref
    ([] : (linter * (file:file -> Cmi_format.cmi_infos -> unit)) list )

let messages = ref []

let get_messages () =
  let ms = List.rev !messages in
  messages := [];
  ms

let new_plugin ?(version="0.1.0") plugin_name =
  for i = 0 to String.length plugin_name - 1 do
    match plugin_name.[i] with
    | 'A'..'Z' -> ()
    | '0'..'9' | '_' when i>0 -> ()
    | c ->
       Printf.eprintf "Configuration error: plugin %S contains illegal character '%c'\n%!" plugin_name c;
       exit 2
  done;
  if Hashtbl.mem all_plugins plugin_name then begin
      Printf.eprintf "Configuration error: plugin %s is defined twice.\n%!" plugin_name ;
      Printf.eprintf "  Did you load twice the same plugin ?\n%!";
      exit 2
    end;
  let ns = { plugin_name ;
             plugin_version = version ;
             plugin_warnings = IntMap.empty ;
             plugin_linters = StringMap.empty } in
  Hashtbl.add all_plugins plugin_name ns;
  ns

let new_tag tag_name =
  for i = 0 to String.length tag_name - 1 do
    match tag_name.[i] with
    | 'a'..'z' -> ()
    | '0'..'9' | '_' when i>0 -> ()
    | c ->
       Printf.eprintf "Configuration error: tag_name %S contains illegal character '%c'\n%!" tag_name c;
       exit 2
  done;
  try
    Hashtbl.find all_tags tag_name
  with Not_found ->
    let tag = { tag_name ; tag_warnings = [] } in
    Hashtbl.add all_tags tag_name tag;
    tag

let add_tag w tag =
  w.w_tags <- tag :: w.w_tags ;
  tag.tag_warnings <- w :: tag.tag_warnings ;
  let nstag = Printf.sprintf "%s:%s"
                w.w_plugin.plugin_name tag.tag_name in
  let r =
    try
      Hashtbl.find all_nstags nstag
    with Not_found ->
          let r = ref [] in
          Hashtbl.add all_nstags nstag r;
          r
  in
  r := w :: !r

let new_warning
      w_plugin
      ?(tags=[])
      ?(desc="")
      ~name:w_name
      ~msg:w_msg
      w_num
  =
  let w_idstr = Printf.sprintf "%s+%d"
                  w_plugin.plugin_name  w_num
  in
  let w = {
      w_plugin ;
      w_num ;
      w_idstr ;
      w_name ;
      w_tags = [] ;
      w_msg ;
      w_level_warning = false ;
      w_level_error = false ;
      w_desc = desc ;
    } in
  begin
    match IntMap.find w_num w_plugin.plugin_warnings with
    | exception Not_found -> (* good *) ()
    | w0 ->
       Printf.eprintf
         "Configuration error: in plugin %s, warning %d is defined twice:\n%!"
         w_plugin.plugin_name w_num;
       Printf.eprintf "  * for warning %S\n%!" w0.w_name;
       Printf.eprintf "  * for warning %S\n%!" w.w_name;
       exit 2
  end;
  w_plugin.plugin_warnings <- IntMap.add w_num w w_plugin.plugin_warnings;
  all_warnings := w :: !all_warnings ;
  List.iter (add_tag w) tags;
  w

let rec new_linter
          linter_plugin
          linter_name
          ~level:linter_level
          ~warnings: linter_warnings
          ?(on_begin = fun _ -> ())
          ?(on_open =  fun ~file:_ -> ())
          ?(on_close = fun ~file:_ -> ())
          ?(on_end = fun _ -> ())
          linter_install =
  (* TODO: check uniqueness of linter_name *)
  if StringMap.mem linter_name linter_plugin.plugin_linters then begin
      Printf.eprintf "Configuration warning: plugin %S defines linter %S twice. Renaming it.\n%!"
        linter_plugin.plugin_name
        linter_name ;
      new_linter linter_plugin
        (linter_name ^ "X")
        ~level:linter_level
        ~warnings:linter_warnings
        ~on_begin ~on_open ~on_close ~on_end
        linter_install
    end
  else
    let l = {
        linter_plugin ;
        linter_name ;
        linter_warnings ;
        linter_install ;
        linter_level ;
        linter_active = false ;
        linter_begin = on_begin ;
        linter_open = on_open ;
        linter_close = on_close ;
        linter_end = on_end ;
      } in
    linter_plugin.plugin_linters <- StringMap.add
                               linter_name l
                               linter_plugin.plugin_linters ;
    all_linters := l :: !all_linters

let new_src_file_linter
      plugin
      name
      ~warnings
      ?on_begin ?on_end
      f =
  let linter_install l =
    active_src_file_linters := (l, f) :: !active_src_file_linters;
    active_linters := l :: !active_linters;
  in
  new_linter plugin name ~warnings ?on_begin ?on_end
          linter_install ~level:Linter_src_file

let new_gen_linter active_linters_ref level=
  fun
    plugin
    name
    ~warnings
    ?on_begin ?on_open ?on_close ?on_end
    f ->
  let linter_install l =
    active_linters_ref := (l, f) :: !active_linters_ref ;
    active_linters := l :: !active_linters;
  in
  new_linter plugin name ~warnings ?on_begin ?on_open ?on_close ?on_end
    linter_install ~level

let new_src_line_linter =
  new_gen_linter active_src_line_linters Linter_src_line

let new_src_content_linter =
  new_gen_linter active_src_content_linters Linter_src_content

let new_ast_intf_linter =
  new_gen_linter active_ast_intf_linters Linter_ast_intf

let new_ast_impl_linter =
  new_gen_linter active_ast_impl_linters Linter_ast_impl

let new_tast_intf_linter =
  new_gen_linter active_tast_intf_linters Linter_tast_intf

let new_tast_impl_linter =
  new_gen_linter active_tast_impl_linters Linter_tast_impl

let new_sig_linter =
  new_gen_linter active_sig_linters Linter_sig

(* We suppose that all warnings are now correctly set. *)
let activate_linters () =
  List.iter (fun w ->
      if w.w_level_warning || w.w_level_error then
        active_warnings := StringMap.add w.w_idstr w !active_warnings
    ) !all_warnings ;
  List.iter (fun l ->
      if List.exists (fun w ->
             w.w_level_warning || w.w_level_error)
           l.linter_warnings then begin
          l.linter_active <- true;
          l.linter_install l
        end
    ) !all_linters

let warn msg_loc ~file ?msg w =
  if w.w_level_error || w.w_level_warning then
    let msg_string =
      match msg with
      | None -> w.w_msg
      | Some msg -> msg
    in
    let msg_idstr =
      Printf.sprintf "%06d%06d%s"
        msg_loc.loc_start.pos_lnum
        msg_loc.loc_start.pos_cnum
        (Marshal.to_string (msg_loc,w.w_idstr,msg) [])
    in
    let m = {
        msg_loc ;
        msg_string ;
        msg_warning = w;
        msg_file = file ;
        msg_idstr ;
      } in
    file.file_messages <- StringMap.add msg_idstr m file.file_messages ;
    messages := m :: !messages

let mkloc ~bol ?(start_cnum=bol) ?(end_cnum=start_cnum)
      ~lnum ~file () =
  let loc_start =
    Lexing.{
        pos_fname = file.file_name ;
        pos_lnum =  lnum ;
        pos_bol = bol ;
        pos_cnum = start_cnum ;
    }
  in
  let loc_end =
    Lexing.{ loc_start with
             pos_cnum = end_cnum }
  in
  Location.{
      loc_start ;
      loc_end ;
      loc_ghost = true;
  }

let iter_linters ~file linters x =
  List.iter (fun (l, f) ->
      try f ~file x with
      | exn ->
         Printf.eprintf "Configuration warning: linter %S raised exception %S while linting file %S\n%!"
           l.linter_name
           (Printexc.to_string exn)
           file.file_name
    ) linters

let iter_linters_open ~file linters =
  List.iter (fun (l,_) ->
      l.linter_open ~file ;
    ) linters

let iter_linters_close ~file linters =
  List.iter (fun (l,_) ->
      l.linter_close ~file ;
    ) linters

let rec filter_linters ~file linters = 
  match linters with
  | []  -> []
  | (l,f) :: linters ->
     if List.exists (fun w ->
            (w.w_level_warning || w.w_level_error) &&
              not (StringSet.mem w.w_idstr file.file_warnings_done)
          ) l.linter_warnings then
       (l,f) :: filter_linters ~file linters
     else
       filter_linters ~file linters

let lint_src_file ~file =
  let file_name = file.file_name in
  let file_loc = mkloc ~bol:0 ~lnum:0 ~end_cnum:0 ~file () in

  let active_src_file_linters =
    filter_linters ~file !active_src_file_linters in
  let active_src_line_linters =
    filter_linters ~file !active_src_line_linters in
  let active_src_content_linters =
    filter_linters ~file !active_src_content_linters in

  iter_linters_open ~file active_src_file_linters ;
  iter_linters_open ~file active_src_line_linters ;
  iter_linters_open ~file active_src_content_linters ;

  begin
    match active_src_file_linters with
    | [] -> ()
    | linters ->
       iter_linters ~file linters  { file_loc }
  end;

  begin
    match active_src_line_linters,
          active_src_content_linters with
    | [], [] -> ()
    | src_line_linters, src_content_linters ->
       match Ez_file.V1.EzFile.read_file file_name with
       | exception exn ->
          Printf.eprintf
            "Configuration error: could not read file %S, exception %s\n%!" file_name (Printexc.to_string exn)
       | s ->
          iter_linters ~file src_content_linters
            { content_loc = file_loc ; content_string = s };

          let len = String.length s in
          let rec iter lnum pos0 =
            match String.index_from s pos0 '\n' with
            | pos2 ->
               let pos1 =
                 if pos2>pos0 && s.[pos2-1] = '\r' then
                   pos2-1
                 else
                   pos2
               in
               let line_line = String.sub s pos0 (pos1-pos0) in
               let line_sep = String.sub s pos1 (pos2-pos1+1) in
               let line_loc =
                 mkloc ~bol:pos0 ~lnum ~end_cnum:pos2 ~file () in
               iter_linters ~file src_line_linters
                 { line_loc ; line_line ; line_sep };
               iter (lnum+1) (pos2+1)
            | exception _ ->
               if pos0 < len then
                 let line_line = String.sub s pos0 (len-pos0) in
                 let line_sep = "" in
                 let line_loc = mkloc ~bol:pos0 ~lnum
                                  ~end_cnum:len ~file () in
                 iter_linters ~file src_line_linters
                   { line_loc; line_line; line_sep; }
          in
          iter 1 0;
  end;

  iter_linters_close ~file active_src_content_linters ;
  iter_linters_close ~file active_src_line_linters ;
  iter_linters_close ~file active_src_file_linters ;
  ()

let lint_with_active_linters active_linters_ref =
  fun ~file x ->
  match filter_linters ~file !active_linters_ref with
  | [] -> ()
  | linters ->
     iter_linters_open ~file linters ;
     iter_linters ~file linters x ;
     iter_linters_close ~file linters ;
     ()

let lint_ast_intf =
  lint_with_active_linters active_ast_intf_linters

let lint_ast_impl =
  lint_with_active_linters active_ast_impl_linters

let lint_tast_intf =
  lint_with_active_linters active_tast_intf_linters

let lint_tast_impl =
  lint_with_active_linters active_tast_impl_linters

let lint_sig =
  lint_with_active_linters active_sig_linters


