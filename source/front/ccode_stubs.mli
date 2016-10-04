(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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
(* front *)
external systemInit        : int    -> int  = "c_systemInit"
external simulatorInit     : unit   -> int  = "c_simulatorInit"
external simulatorReInit   : bool   -> int  = "c_simulatorReInit"
external link              : string -> int  = "c_link"
external load              : string -> int -> int = "c_load"
external topModuleInstall  : unit   -> int  = "c_topModuleInstall"
external moduleInstall     : int    -> int  = "c_moduleInstall"
external initModuleContext : unit   -> int  = "c_initModuleContext"
external cleanModule       : unit   -> unit = "c_cleanModule"
external setPath           : string -> int  = "c_setPath"


(* query *)
external setTypeAndTermLocation : unit -> unit = "c_setTypeAndTermLocation"
external solveQuery             : unit -> int  = "c_solveQuery"
external showAnswers            : unit -> int  = "c_showAnswers"
external setQueryFreeVariables  : unit -> unit = "c_setQueryFreeVariables"
external queryHasVars           : unit -> bool = "c_queryHasVars"

(* read term *)
external initLocalTabs  : int -> int -> int -> int -> int = "c_initLocalTabs"
external cleanLocalTabs : unit -> unit = "c_cleanLocalTabs" 

external buildFreeVariable     : string -> int -> int = "c_buildFreeVariable"
external buildFreeTypeVariable : int -> int = "c_buildFreeTypeVariable"

external buildIntTerm       : int    -> int  = "c_buildIntTerm"
external buildRealTerm      : float  -> int  = "c_buildRealTerm"
external buildStringTerm    : string -> int  = "c_buildStringTerm"
external buildNilTerm       : unit   -> int  = "c_buildNilTerm"
external buildMConstantTerm : int    -> int  = "c_buildMConstantTerm"
external buildPConstantTerm : int -> int -> int = "c_buildPConstantTerm"
external buildFreeVarTerm   : int -> int = "c_buildFreeVarTerm"
external buildDBTerm        : int -> int = "c_buildDBTerm"
external buildAbstractionTerm : int  -> int = "c_buildAbstractionTerm"
external buildConsTerm        : unit -> int = "c_buildConsTerm"
external buildApplicationTerm : int  -> int = "c_buildApplicationTerm"

external buildArrowType : unit -> int = "c_buildArrowType"
external buildSortType  : int  -> int = "c_buildSortType"
external buildStrType   : int  -> int -> int = "c_buildStrType"
external buildFreeVarType : int -> int = "c_buildFreeVarType" 


(* build (OCaml) term from C representation*)
type tm_ptr
  (* returns the number of variables that were in
     the original query and need to be included in
     the answer substitution *)
external getNumQueryVars : unit -> int = "c_numQueryVars"

  (* reset the free variable table to just the query vars. *)
external resetFreeVarTab : unit -> unit = "c_resetFreeVarTab"

  (* returns the term substituted for the variable 
     at the given index in the free variable table. *)
external getSubTerm : int -> tm_ptr = "c_getSubTerm"

  (* returns the tag on the given term *)
external getTermTag : tm_ptr -> int = "c_getTermTag"

  (* return disagreement set as list*)
external getDisSet : unit -> ((tm_ptr * tm_ptr) list) = "c_getDisSet"

(* constants *)
  (* returns the index into constant table of given constant term *)
external getConstData : tm_ptr -> int = "c_getConstData"

(* free variables *)
  (* returns index into the free variable table if found,
     else returns IO_freeVarTabTop *)
external getFVarData : tm_ptr -> int = "c_getFVarData"

  (* add the newly generated free variable name to the free var table in memory *)
external setFVarTabName : int -> string -> unit = "c_setFVarName"

(* bound variables *)
  (* return de Bruijn index of given bound var term *)
external getBVarData : tm_ptr -> int = "c_getBVarData"

(* abstractions *)
  (* returns tuple with the number of abstractions and body of abstraction *)
external getAbsData : tm_ptr -> int * tm_ptr = "c_getAbsData"

(* application *)
  (* returns tuple of application head, argument list, and number of arguments *)
external getAppData : tm_ptr -> tm_ptr * (tm_ptr list) * int = "c_getAppData"


  (*** used to get the different tag values needed
       for building a term ***)
external getConstTag : unit -> int = "c_getConstTag"
external getFVarTag : unit -> int = "c_getFVarTag"
external getBVarTag : unit -> int = "c_getBVarTag"
external getAbsTag : unit -> int = "c_getAbsTag"
external getAppTag : unit -> int = "c_getAppTag"
