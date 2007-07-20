#ifndef _LD_MESSAGE_H_
#define _LD_MESSAGE_H_

#include <stdio.h>

extern int LD_verbosity;

#define LD_debug(args...) if(LD_verbosity>2) fprintf(stderr,args)

#define LD_detail(args...) if(LD_verbosity>1) fprintf(stderr,args)

#define LD_mutter(args...) if(LD_verbosity>0) fprintf(stderr,args)

#define LD_bad(args...) fprintf(stderr,args)

#endif
