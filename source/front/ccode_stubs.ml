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
(* NG: No longer in use, since queries are now compiled *)
(* external setTypeAndTermLocation : unit -> unit = "c_setTypeAndTermLocation" *)
external solveQuery             : unit -> int  = "c_solveQuery"
external showAnswers            : unit -> int  = "c_showAnswers"
external setQueryFreeVariables  : int -> unit = "c_setQueryFreeVariables"
external queryHasVars           : unit -> bool = "c_queryHasVars"
external loadQuery              : string -> unit = "c_loadQuery"
external setQueryEntryPoint     : int -> unit = "c_setQueryEntryPoint"

(* pipes *)
external openPipe  : unit -> unit = "c_openPipe"
external getPipeIn : unit -> Unix.file_descr = "c_getPipeIn"
                                         
                                              
(* read term *)
(* NG: No longer in use, since queries are now compiled *)
(* external initLocalTabs  : int -> int -> int -> int -> int = "c_initLocalTabs" *)
external initLocalTabsQuery : int -> int = "c_initLocalTabsQuery"
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

