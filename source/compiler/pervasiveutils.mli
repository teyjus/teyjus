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
val isOverloaded : Absyn.aconstant -> bool
val getOverload : Absyn.akind -> Absyn.aconstant -> Absyn.aconstant
val maxSkeletonIndex : int

val listSeparatorConstant : Absyn.aconstant
val cutFailTerm : Absyn.aterm 

(**************************************************************************)
(* pervasive kind and constant index-data mapping table: needed for       *)
(* loading and disassembling.                                             *)
(**************************************************************************)
val pervasiveKindIndexMappingInit : unit -> unit
val findKindIndexMapping : int -> Absyn.akind option

val pervasiveConstantIndexMappingInit : unit -> unit
val findConstantIndexMapping : int -> Absyn.aconstant option
