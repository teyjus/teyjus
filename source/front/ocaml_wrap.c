//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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


void ocaml_simulator_main(void)
{
  static value *ocaml_main_closure = NULL;
  if (ocaml_main_closure == NULL) 
    ocaml_main_closure = caml_named_value("ocaml_simulator_main");
  caml_callback(*ocaml_main_closure, Val_unit);
}

int ocaml_read_term_stdin(void)
{
  static value *ocaml_read_term_stdin_closure = NULL;
  if (ocaml_read_term_stdin_closure == NULL) 
    ocaml_read_term_stdin_closure = caml_named_value("ocaml_read_term_stdin");
  return Int_val(caml_callback(*ocaml_read_term_stdin_closure, Val_unit));
}

int ocaml_read_term_file_id(char *fname)
{
  static value *ocaml_read_term_file_id_closure = NULL;
  if (ocaml_read_term_file_id_closure == NULL) 
    ocaml_read_term_file_id_closure =
	    caml_named_value("ocaml_read_term_file_id");
  return Int_val(caml_callback(*ocaml_read_term_file_id_closure, 
			  	caml_copy_string(fname)));
}

int ocaml_open(char *str, char *mode)
{
  static value *ocaml_open_closure = NULL;
  if (ocaml_open_closure == NULL) 
    ocaml_open_closure = caml_named_value("ocaml_open");
  return Int_val(caml_callback2(*ocaml_open_closure, caml_copy_string(str),
			  	caml_copy_string(mode)));
}

int ocaml_close(char *fname)
{
  static value *ocaml_close_closure = NULL;
  if (ocaml_close_closure == NULL) 
    ocaml_close_closure = caml_named_value("ocaml_close");
  return Int_val(caml_callback(*ocaml_close_closure, caml_copy_string(fname)));
}

char * ocaml_input_line_stdin(void)
{
  static value *ocaml_input_line_stdin_closure = NULL;
  if (ocaml_input_line_stdin_closure == NULL) 
    ocaml_input_line_stdin_closure = caml_named_value("ocaml_input_line_stdin");
  return String_val(caml_callback(*ocaml_input_line_stdin_closure, Val_unit));
}
