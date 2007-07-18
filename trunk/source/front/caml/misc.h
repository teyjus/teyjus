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

/* $Id: misc.h,v 1.30 2004/05/17 17:09:59 doligez Exp $ */

/* Miscellaneous macros and variables. */

#ifndef CAML_MISC_H
#define CAML_MISC_H

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "config.h"

/* Standard definitions */

#include <stddef.h>
#include <stdlib.h>

/* Basic types and constants */

typedef size_t asize_t;

#ifndef NULL
#define NULL 0
#endif


#ifdef __GNUC__
  /* Works only in GCC 2.5 and later */
  #define Noreturn __attribute__ ((noreturn))
#else
  #define Noreturn
#endif

/* Export control (to mark primitives and to handle Windows DLL) */

#if defined(_WIN32) && defined(CAML_DLL)
# define CAMLexport __declspec(dllexport)
# define CAMLprim __declspec(dllexport)
# if defined(IN_OCAMLRUN)
#  define CAMLextern __declspec(dllexport) extern
# else
#  define CAMLextern __declspec(dllimport) extern
# endif
#else
# define CAMLexport
# define CAMLprim
# define CAMLextern extern
#endif

/* Assertions */


#endif /* CAML_MISC_H */
