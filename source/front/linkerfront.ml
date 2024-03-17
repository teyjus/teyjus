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

external link : string -> int -> int = "FRONT_link"

let verbosity = ref 0
  
let moreVerbose () = incr verbosity

let specList = multLine
  [("-V", "--verbose", Arg.Unit moreVerbose,
    " Produce verbose output - use multiple times to increase verbosity", " \n");
   versionspec]

let usageMsg =
  "Usage: tjlink [options] <module-name>\n" ^
  "options are:"

let _ =
  (Arg.parse (Arg.align specList) (setInputName ~filter:getModName) usageMsg ;
  ensureInputName () ;
  exit (link !inputName !verbosity))
