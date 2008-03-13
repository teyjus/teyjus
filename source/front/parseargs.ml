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
let dualArgs dualSpecList =
  let seperate (key1, key2, spec, doc) =
    [(key1, spec, doc) ; (key2, spec, doc)]
  in
    List.flatten (List.map seperate dualSpecList)
    
let printVersion () =
  print_string "version number:... \n"

let versionspec =
  ("-v", "--version", Arg.Unit printVersion, " Return the system version")

let getModName name =
  try
    Filename.chop_extension name
  with
    | Invalid_argument _ -> name

let error str =
  prerr_endline ("Error : " ^ str) ;
  exit 1
