#ifndef _LD_MESSAGE_H_
#define _LD_MESSAGE_H_

#include <stdio.h>

extern int LD_verbosity;

#define LD_debug(...) {if(LD_verbosity>2){fprintf(stderr,__VA_ARGS__);}}

#define LD_detail(...) {if(LD_verbosity>1){fprintf(stderr,__VA_ARGS__);}}

#define LD_mutter(...) {if(LD_verbosity>0){fprintf(stderr,__VA_ARGS__);}}

#define LD_error(...) {fprintf(stderr,__VA_ARGS__);}

#endif
