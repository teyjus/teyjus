//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////

#include "caml/mlvalues.h"
#include "caml/callback.h"
#include "caml/alloc.h"

// NG: No longer in use, since queries are now compiled
/* void ocaml_simulator_main() */
/* { */
/*   static value *ocaml_main_closure = NULL; */
/*   if (ocaml_main_closure == NULL)  */
/*     ocaml_main_closure = caml_named_value("ocaml_simulator_main"); */
/*   caml_callback(*ocaml_main_closure, Val_unit); */
/* } */

int ocaml_read_term(char *str)
{
  static value *ocaml_read_term_closure = NULL;
  if (ocaml_read_term_closure == NULL)
    ocaml_read_term_closure = caml_named_value("ocaml_read_term");
  return Int_val(caml_callback(*ocaml_read_term_closure, caml_copy_string(str)));
}
