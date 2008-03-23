(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xioachu Qi, Zach Snow
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
(**********************************************************************
*Compile:
* An interface to the lexer and parser.
**********************************************************************)
val typeAbbrevTable : Preabsyn.ptypeabbrev Table.SymbolTable.t

(**********************************************************************
*compileModule:
* Given the filename of a lambda prolog module, parses the file and
* returns a preabsyn module.
**********************************************************************)
val compileModule : string -> Preabsyn.pmodule

(**********************************************************************
*compileModule:
* Given the filename of a lambda prolog signature, parses the file and
* returns a preabsyn signature.
**********************************************************************)
val compileSignature : string -> Preabsyn.pmodule

(**********************************************************************
*compileModule:
* Given a string containing lambda prolog code, parses the file and
* returns a preabsyn term.
**********************************************************************)
val compileString : string -> Preabsyn.pterm option

(*  Flags indicating whether to print various stages of compilation;
    for testing.  *)
val printPreAbsyn : bool ref
val printAbsyn : bool ref
val printClauses : bool ref

val openFile : string -> (string -> 'a) -> 'a
val closeFile : 'a -> ('a -> unit) -> unit
