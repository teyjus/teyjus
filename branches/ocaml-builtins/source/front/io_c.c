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

#include "io_c.h"
#include "ocaml_wrap.h"

int FRONT_IO_readTermAndTypeStdin(void)
{
  return ocaml_read_term_stdin();
}

int FRONT_IO_readTermAndTypeFileId(int file_id)
{
  return ocaml_read_term_file_id(file_id);
}

int FRONT_IO_open(char *fname, char *mode)
{
  return ocaml_open(fname, mode);
}

int FRONT_IO_close(int file_id)
{
  return ocaml_close(file_id);
}
