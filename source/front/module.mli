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
(***************************************************************************)
(*            link and load a module                                       *)
(***************************************************************************)
val moduleLoad : string -> unit
val setPath    : string -> unit

(***************************************************************************)
(*       install and open a module context                                 *)
(* 1. search the ocaml module table to find the module and its index;      *)
(* 2. register the module as current module;                               *)
(* 3. invoke C functions to install the module and open its context for the*)
(*    simulator.                                                           *)
(***************************************************************************)
val moduleInstall     : string -> unit
val initModuleContext : unit   -> unit
val getCurrentModule  : unit   -> Absyn.amodule


val cleanModule       : unit   -> unit
