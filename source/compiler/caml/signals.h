/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: signals.h,v 1.21 2004/01/01 16:42:37 doligez Exp $ */

#ifndef CAML_SIGNALS_H
#define CAML_SIGNALS_H

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"


CAMLextern void caml_enter_blocking_section (void);
CAMLextern void caml_leave_blocking_section (void);


#endif /* CAML_SIGNALS_H */
