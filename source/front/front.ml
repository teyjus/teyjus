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
(** ********************************************************************** **)
(**             system initialization                                      **)
(** ********************************************************************** **)

(****************************************************************************)
(* initialization code for OCaml part of the system                         *)
(****************************************************************************)
let systemInitOCaml () =
  (* set the number of bytes contained by a word *)
  Bytecode.setWordSize (); 
  (* set pervasive kind and index mapping: needed for ocaml loader *)
  Pervasiveutils.pervasiveKindIndexMappingInit ();
  Pervasiveutils.pervasiveConstantIndexMappingInit ()

(****************************************************************************)
(* invocation of the initialization code for the C part of the system       *)
(****************************************************************************)
let systemInitC heapSize =
  Simerrors.handleSimExceptions (Ccode_stubs.systemInit heapSize)

(***************************************************************************)
(* interface function                                                      *)
(***************************************************************************)
let systemInit heapSize =
  systemInitOCaml () ;
  systemInitC heapSize

(** ********************************************************************** **)
(**            simulator initialization                                    **)
(** ********************************************************************** **)
let simulatorInit () =
  Simerrors.handleSimExceptions (Ccode_stubs.simulatorInit ())
  
let simulatorReInit imp =
  Simerrors.handleSimExceptions (Ccode_stubs.simulatorReInit imp)
