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

/*****************************************************************************/
/*                                                                           */
/* builtins/io.h{c}                                                          */
/*****************************************************************************/

#ifndef IO_H
#define IO_H

void BIIO_openIn();
void BIIO_openOut();
void BIIO_openApp();
void BIIO_closeIn();
void BIIO_closeOut();
void BIIO_openStr();
void BIIO_input();
void BIIO_output();
void BIIO_inputLine();
void BIIO_lookahead();
void BIIO_eof();
void BIIO_flush();
void BIIO_print();
void BIIO_read();
void BIIO_printTerm();
void BIIO_termToStr();
void BIIO_strToTerm();
void BIIO_readTerm();
void BIIO_getEnv();
void BIIO_openSocket();
void BIIO_unixTime();
void BIIO_system();

#endif  //IO_H
