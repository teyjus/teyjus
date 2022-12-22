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
let isOverloaded c =
  (Pervasive.isoverloadUMinusConstant c) ||
  (Pervasive.isoverloadAbsConstant c) || 
  (Pervasive.isoverloadPlusConstant c) ||
  (Pervasive.isoverloadMinusConstant c) ||
  (Pervasive.isoverloadTimeConstant c) ||
  (Pervasive.isoverloadLTConstant c) ||
  (Pervasive.isoverloadGTConstant c) ||
  (Pervasive.isoverloadLEConstant c) ||
  (Pervasive.isoverloadGEConstant c)

let getIntOverload c =
  if Pervasive.isoverloadUMinusConstant c then
    Pervasive.intuminusConstant
  else if Pervasive.isoverloadPlusConstant c then
    Pervasive.intplusConstant
  else if Pervasive.isoverloadMinusConstant c then
    Pervasive.intminusConstant
  else if Pervasive.isoverloadTimeConstant c then
    Pervasive.intmultConstant
  else if Pervasive.isoverloadAbsConstant c then
    Pervasive.iabsConstant 
  else if Pervasive.isoverloadLTConstant c then
    Pervasive.intlssConstant
  else if Pervasive.isoverloadGTConstant c then
    Pervasive.intgrtConstant
  else if Pervasive.isoverloadLEConstant c then
    Pervasive.intleqConstant
  else if Pervasive.isoverloadGEConstant c then
    Pervasive.intgeqConstant
  else
    (Errormsg.impossible Errormsg.none "PervasiveUtils.getIntOverload: invalid constant")

let getRealOverload c =
  if Pervasive.isoverloadUMinusConstant c then
    Pervasive.realuminusConstant
  else if Pervasive.isoverloadPlusConstant c then
    Pervasive.realplusConstant
  else if Pervasive.isoverloadMinusConstant c then
    Pervasive.realminusConstant
  else if Pervasive.isoverloadTimeConstant c then
    Pervasive.realmultConstant
  else if Pervasive.isoverloadAbsConstant c then
    Pervasive.rabsConstant 
  else if Pervasive.isoverloadLTConstant c then
    Pervasive.reallssConstant
  else if Pervasive.isoverloadGTConstant c then
    Pervasive.realgrtConstant
  else if Pervasive.isoverloadLEConstant c then
    Pervasive.realleqConstant
  else if Pervasive.isoverloadGEConstant c then
    Pervasive.realgeqConstant
  else
    (Errormsg.impossible Errormsg.none "PervasiveUtils.getRealOverload: invalid constant")

let getStringOverload c =
  if Pervasive.isoverloadLTConstant c then
    Pervasive.strlssConstant
  else if Pervasive.isoverloadGTConstant c then
    Pervasive.strgrtConstant
  else if Pervasive.isoverloadLEConstant c then
    Pervasive.strleqConstant
  else if Pervasive.isoverloadGEConstant c then
    Pervasive.strgeqConstant
  else
    (Errormsg.impossible Errormsg.none "PervasiveUtils.getStringOverload: invalid constant")

let getOverload k c =
  if Pervasive.iskint k then
    getIntOverload c
  else if Pervasive.iskreal k then
    getRealOverload c
  else if Pervasive.iskstring k then
    getStringOverload c
  else
    c

let maxSkeletonIndex = 256

(*  This constant is used in Parse.ml to temporarily represent the list
    separator comma.  *)
let listSeparatorConstant =
  let tyskel = Absyn.Skeleton(
    Absyn.ArrowType(
      Absyn.SkeletonVarType(ref 0),
      Absyn.ArrowType(
        Absyn.SkeletonVarType(ref 0),
        Absyn.SkeletonVarType(ref 0))),
    ref None,
    ref false)
  in
      
  Absyn.Constant((Symbol.symbol ","), (ref Absyn.Infixr), (ref min_int), (ref false),
    (ref false), (ref false), (ref false), (ref true), (ref false), (ref (Some tyskel)),
    (ref 1), (ref (Some (Array.make 1 true))),
    (ref (Some (Array.init 1 (fun x -> if x >= 0 then false else true)))),
    (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 0), Errormsg.none)

let cutFailTerm = 
  Absyn.ApplicationTerm(Absyn.FirstOrderApplication(
						Absyn.ConstantTerm(Pervasive.andConstant, [],  Errormsg.none),
						[Absyn.ConstantTerm(Pervasive.cutConstant, [], Errormsg.none) ;
						 Absyn.ConstantTerm(Pervasive.failConstant, [], Errormsg.none)], 2),
						 Errormsg.none)

(**************************************************************************)
(* pervasive kind and constant index-data mapping table: needed for       *)
(* loading and disassembling.                                             *)
(**************************************************************************)
let pervasiveKindIndexMapping : ((Absyn.akind option) array) ref =
  ref (Array.make Pervasive.numberPervasiveKinds None)

let getPervasiveKindIndexMapping () = !pervasiveKindIndexMapping
let setPervasiveKindIndexMapping m  = pervasiveKindIndexMapping := m

(* fill in the pervasive kinds index-kind data mapping:                   *)
(* Note only those appearing in the run-time symbol table (which are      *)
(* assumed to have indexes smaller than "numberPervasivekinds") are needed*)
(* and filled in.                                                         *)   
let pervasiveKindIndexMappingInit () =
  let kindIndexMapping = getPervasiveKindIndexMapping () in

  let collectKindIndexMapping symbol kind =
	let index = Absyn.getKindIndex kind in
	if (index < Pervasive.numberPervasiveKinds) then
	  Array.set kindIndexMapping index (Some kind)
	else
	  ()
  in
  Table.iter collectKindIndexMapping Pervasive.pervasiveKinds;
  setPervasiveKindIndexMapping kindIndexMapping
  
(* find kind data from the mapping table through its index *) 
let findKindIndexMapping index =
  Array.get (getPervasiveKindIndexMapping ()) index

let pervasiveConstantIndexMapping : ((Absyn.aconstant option) array) ref =
  ref (Array.make Pervasive.numberPervasiveConstants None)
	
let getPervasiveConstantIndexMapping () = !pervasiveConstantIndexMapping
let setPervasiveConstantIndexMapping m  = pervasiveConstantIndexMapping := m

(* fill in the pervasive constants index-constant data mapping:           *)
(* Note only those appearing in the run-time symbol table (which are      *)
(* assumed to have indexes smaller than "numberPervasivekinds") are needed*)
(* and filled in.                                                         *)   
let pervasiveConstantIndexMappingInit () =
  let constIndexMapping = getPervasiveConstantIndexMapping () in
  
  let collectConstIndexMapping symbol const =
	let index = Absyn.getConstantIndex const in
	if (index < Pervasive.numberPervasiveConstants) then
	  Array.set constIndexMapping index (Some const)
	else
	  ()
		
  in
  Table.iter collectConstIndexMapping Pervasive.pervasiveConstants;
  setPervasiveConstantIndexMapping constIndexMapping

(* find constant data from the mapping table through its index *)
let findConstantIndexMapping index =
  Array.get (getPervasiveConstantIndexMapping ()) index
