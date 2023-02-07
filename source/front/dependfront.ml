(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Nathan Guermond, Steven Holte, 
*  Gopalan Nadathur, Xiaochu Qi, Zach Snow
****************************************************************************)
(****************************************************************************
* This file is part of Teyjus.
*
* Teyjus is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Teyjus is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************)
open Parseargs

module H = Hashtbl

let abortOnError () =
  if !Errormsg.anyErrors then
    exit 1

let unique list =
  let rec aux list =
    match list with
      | [] -> []
      | head::tail ->
          let tail' = List.filter (fun x -> x <> head) tail in
            head :: (aux tail')
  in
    aux list
        
let psymbol_to_name ps =
  match ps with
    | Preabsyn.Symbol(s, _, _) -> Symbol.name s

type status =
  | Computing
  | Computed of string list

(*
 * Takes a filename and returns the directory of TJPATH where the file is.
 * Compatible with openFile in compile module 
 *)
let getPath filename =
  (* Writing those few lines avoid linking with Str module *)
  let rec split_char sep str =
    try
      let i = String.index str sep in
        String.sub str 0 i :: split_char sep (String.sub str (i+1)
              (String.length str - i - 1))
     with Not_found -> [str] in
  match Sys.file_exists filename with
    | true -> ""
    | false ->
      try
        let tjpath = Sys.getenv "TJPATH" in
        let dirs = split_char ':' tjpath in
        try match List.find (fun dir -> 
          Sys.file_exists (Filename.concat dir filename)) dirs 
        with
          | d -> d
	with Not_found -> (* The file does not exist in TJPATH *)
	  (prerr_endline ("File " ^ filename ^ " does not exist in TJPATH."); exit 1)
      with Not_found -> (* TJPATH is not defined *)
	(prerr_endline ("File " ^ filename ^ " not found. Did you forget to set TJPATH?"); exit 1)


(* Dependency of lp file on lpo files *)

let lpo_deps_table = H.create 10

let compile_module modname =
  let result = Compile.compileModule modname in
    abortOnError () ;
    result

let rec lpo_deps modname =
  try match H.find lpo_deps_table modname with
    | Computing -> failwith ("Cycle detected in module: " ^ modname)
    | Computed deps -> deps
  with
    | Not_found ->
        H.add lpo_deps_table modname Computing ;
        match compile_module modname with
          | Preabsyn.Module(_, _, _, _, _, _, _, _, _, _, _,
                            accum_mods, accum_sigs, use_sigs, import_mods) ->
              let direct_deps =
                List.map psymbol_to_name (accum_mods @ import_mods)
              in
              let nested_deps =
                List.flatten (List.map lpo_deps direct_deps)
              in
              let all_deps =
                unique (direct_deps @ nested_deps)
              in
                H.replace lpo_deps_table modname (Computed all_deps) ;
                all_deps
          | _ -> assert false

let print_lpo_deps modname =
  let deps = lpo_deps modname in
  let deps_path = List.map (fun m -> getPath (m ^ ".mod")) deps in
  let deps_str = String.concat " " (List.map2 (fun p d -> (Filename.concat p d) ^ ".lpo") deps_path deps) in
    Printf.printf "%s.lp: %s.lpo %s\n" modname modname deps_str


(* Dependency of lpo file on sig files *)

let sig_deps_table = H.create 10

let compile_signature signame =
  let result = Compile.compileSignature signame in
    abortOnError () ;
    result

let rec sig_deps signame =
  try match H.find sig_deps_table signame with
    | Computing -> failwith ("Cycle detected in signature: " ^ signame)
    | Computed deps -> deps
  with
    | Not_found ->
        H.add sig_deps_table signame Computing ;
        match compile_signature signame with
          | Preabsyn.Signature(_, _, _, _, _, _, _, accum_sigs, use_sigs) ->
              let direct_deps =
                List.map psymbol_to_name (accum_sigs @ use_sigs)
              in
              let nested_deps =
                List.flatten (List.map sig_deps direct_deps)
              in
              let all_deps =
                unique (direct_deps @ nested_deps)
              in
                H.replace sig_deps_table signame (Computed all_deps) ;
                all_deps
          | _ -> assert false

let root_sig_deps modname =
  let sig_deps_from_mod =
    match compile_module modname with
      | Preabsyn.Module(_, _, _, _, _, _, _, _, _, _, _,
                        accum_mods, accum_sigs, use_sigs, import_mods) ->
          let direct_deps =
            List.map psymbol_to_name
              (accum_mods @ accum_sigs @ use_sigs @ import_mods)
          in
          let nested_deps =
            List.flatten (List.map sig_deps direct_deps)
          in
            unique (direct_deps @ nested_deps)
      | _ -> assert false
  in
  let sig_deps_from_sig = sig_deps modname in
    unique (sig_deps_from_mod @ sig_deps_from_sig)

let print_sig_deps signame =
  let deps = root_sig_deps signame in
  let deps_path = List.map (fun m -> getPath (m ^ ".sig")) deps in
  let deps_str = String.concat " " (List.map2 (fun p d -> (Filename.concat p d) ^ ".sig") deps_path deps) in
    Printf.printf "%s.lpo: %s.mod %s.sig %s\n" signame signame signame deps_str

      
(* Actual front-end code *)
      
let usageMsg = 
  "Usage: tjdepend [options] <module-files>\n" ^
  "options are:"

let specList = multLine [versionspec]

let inputNames = ref []

let addInputName name =
  inputNames := (getModName name) :: !inputNames

let _ =    
  Arg.parse (Arg.align specList) addInputName usageMsg ;

  List.iter print_sig_deps !inputNames ;
  List.iter print_lpo_deps !inputNames

