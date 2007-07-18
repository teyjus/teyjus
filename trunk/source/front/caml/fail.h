/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: fail.h,v 1.25 2004/01/02 19:23:20 doligez Exp $ */

#ifndef CAML_FAIL_H
#define CAML_FAIL_H


#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "misc.h"
#include "mlvalues.h"


CAMLextern void caml_raise (value bucket) Noreturn;
CAMLextern void caml_raise_constant (value tag) Noreturn;
CAMLextern void caml_raise_with_arg (value tag, value arg) Noreturn;
CAMLextern void caml_raise_with_string (value tag, char * msg) Noreturn;
CAMLextern void caml_failwith (char *) Noreturn;
CAMLextern void caml_invalid_argument (char *) Noreturn;
CAMLextern void caml_raise_out_of_memory (void) Noreturn;
CAMLextern void caml_raise_stack_overflow (void) Noreturn;
CAMLextern void caml_raise_sys_error (value) Noreturn;
CAMLextern void caml_raise_end_of_file (void) Noreturn;
CAMLextern void caml_raise_zero_divide (void) Noreturn;
CAMLextern void caml_raise_not_found (void) Noreturn;
CAMLextern void caml_init_exceptions (void);
CAMLextern void caml_array_bound_error (void) Noreturn;
CAMLextern void caml_raise_sys_blocked_io (void) Noreturn;

#endif /* CAML_FAIL_H */
