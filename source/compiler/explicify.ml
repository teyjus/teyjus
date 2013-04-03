(****************************************************************************
*Copyright 2008, 2013
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Fabien Renaud,
*  Zach Snow
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
open Preabsyn

let explicify_name name = name 
let explicify_gconsts gconsts = gconsts 
let explicify_lconsts lsconsts = lsconsts 
let explicify_cconsts cconsts = cconsts 
let explicify_uconsts uconsts = uconsts 
let explicify_econsts econsts = econsts 
let explicify_fixities fixities = fixities 
let explicify_gkinds gkinds = gkinds  
let explicify_lkinds lkinds = lkinds  
let explicify_tabbrevs tabbrevs = tabbrevs 
let explicify_clauses clauses = clauses 
let explicify_accummods accummods = accummods 
let explicify_accumsigs accumsigs = accumsigs
let explicify_usesigs usesigs = usesigs 



let explicify pmodule = match pmodule with 
  | Module(name, gconsts, lconsts, cconsts, uconsts, econsts, fixities,
      gkinds, lkinds, tabbrevs, clauses, accummods,
      accumsigs, usesigs, impmods) -> 
      let name' = explicify_name name in 
      let gconsts' = explicify_gconsts gconsts in 
      let lconsts' = explicify_lconsts lconsts in 
      let cconsts' = explicify_cconsts cconsts in 
      let uconsts' = explicify_uconsts uconsts in 
      let econsts' = explicify_econsts econsts in 
      let fixities' = explicify_fixities fixities in 
      let gkinds' = explicify_gkinds gkinds in 
      let lkinds' = explicify_lkinds lkinds in 
      let tabbrevs' = explicify_tabbrevs tabbrevs in 
      let clauses' = explicify_clauses clauses in 
      let accummods' = explicify_accummods accummods in 
      let accumsigs' = explicify_accumsigs accumsigs in 
      let usesigs' = explicify_usesigs usesigs in 
      (* Importation of modules is deprecated *)
      let impmods' = impmods in 
        Module(name', gconsts', lconsts', cconsts', uconsts', econsts', fixities',
               gkinds', lkinds', tabbrevs', clauses', accummods',
               accumsigs', usesigs', impmods') 
  | s -> s (* TODO *)
