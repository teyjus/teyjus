/****************************************************************************
 *                                                                          *
 *   File signal.h -- code to implement signals and signal handlers for     *
 *   Teyjus. (TEMP)                                                         *
 *                                                                          *
 ****************************************************************************/
#ifndef SIGNAL_H
#define SIGNAL_H

#include <setjmp.h>


/****************************************************************************
 * Different sigsetjmp/siglongjmp depending on support..                    *
 ****************************************************************************/

#define SIGNAL_jmp_buf jmp_buf
#define SIGNAL_setjmp(env) setjmp(env)
#define SIGNAL_longjmp(env, val) longjmp(env, val)


#endif /* SIGNAL_H */
