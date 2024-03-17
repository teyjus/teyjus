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

let tablesOnly = ref false
let instrOnly  = ref false
  
let specList = multLine
  [("-t", "--table", Arg.Set tablesOnly, " Only print tables", " \n") ;
   ("-i", "--instr", Arg.Set instrOnly, " Only print instructions", " \n") ;
   versionspec]

let usageMsg =
  "Using: tjdis [options] <object-file>\n" ^
    "options are:"

let _ =
  Arg.parse (Arg.align specList) setInputName usageMsg ;
  ensureInputName () ;
  
  if (!tablesOnly && !instrOnly) then
    error ("tables only (-t/--table) and instructions only " ^
             "(-i/--instr) can not be selected simultaneously.") ;
  
  Bytecode.setWordSize () ;
  Pervasiveutils.pervasiveKindIndexMappingInit () ;
  Pervasiveutils.pervasiveConstantIndexMappingInit () ;
  Disassembly.disassemble !inputName !tablesOnly !instrOnly
