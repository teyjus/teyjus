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
(**********************************************************************
*Table:
* Tables implemented using balanced binary trees.
**********************************************************************)
module OrderedSymbol =
struct
  type t = Symbol.symbol
  let compare = 
    fun s1 s2 ->
      if((Symbol.id s1) < (Symbol.id s2)) then
        -1
      else if((Symbol.id s1) > (Symbol.id s2)) then
        1
      else
        0
end

module SymbolTable = Map.Make(OrderedSymbol)
type 'a symboltable = 'a SymbolTable.t

let find =
  fun k table ->
    try
      let v = SymbolTable.find k table in
      Some v
    with Not_found -> None

let add = fun k v table ->
  (SymbolTable.add k v table)

let iter = fun f table ->
  (SymbolTable.iter f table)

let fold = fun f v table ->
  (SymbolTable.fold f v table)
  
let empty = SymbolTable.empty


let printTable toStringFunc table =
  let printFunc s ent =
    print_endline (toStringFunc s ent)
  in
  SymbolTable.iter printFunc table

