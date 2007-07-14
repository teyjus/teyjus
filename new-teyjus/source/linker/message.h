#ifndef _LK_MESSAGE_H_
#define _LK_MESSAGE_H_

extern int verbosity;

#define debug(args...) if(verbosity>2) fprintf(stderr,args)

#define detail(args...) if(verbosity>1) fprintf(stderr,args)

#define mutter(args...) if(verbosity>0) fprintf(stderr,args)

#define bad(args...) fprintf(stderr,args)

#endif
