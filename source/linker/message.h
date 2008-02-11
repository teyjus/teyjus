#ifndef _LK_MESSAGE_H_
#define _LK_MESSAGE_H_

extern int verbosity;

#define debug(...) if(verbosity>2){fprintf(stderr,__VA_ARGS__);}

#define detail(...) if(verbosity>1){fprintf(stderr,__VA_ARGS__);}

#define mutter(...) if(verbosity>0){fprintf(stderr,__VA_ARGS__);}

#define bad(...) fprintf(stderr,__VA_ARGS__)
#define error bad

#endif
