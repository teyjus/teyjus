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

#include "../linker/module.h"
#include "../linker/linker_message.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/custom.h"

value FRONT_link(value mod_str, value verb)
{
  CAMLparam2 (mod_str, verb);

  linker_verbosity = Int_val(verb);
  
  EM_TRY
  {
    InitAll();
    LoadTopModule(String_val(mod_str));
    WriteAll(String_val(mod_str));
  }
  EM_CATCH
  {
    bad("Linking aborted due to an exception.\n");
    CAMLreturn(Val_int(1));
  }

  CAMLreturn(Val_int(0));
}
