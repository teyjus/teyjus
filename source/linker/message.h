#ifndef _LK_MESSAGE_H_
#define _LK_MESSAGE_H_

extern int verbosity;

#define mutter(args...) if(verbosity) fprintf(stderr,args)

#define error(args...) fprintf(stderr,args)

#endif
