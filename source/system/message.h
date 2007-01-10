/****************************************************************************
 *                                                                          *
 *   File message.h -- code to present messages to the user in Teyjus.      *
 * supports dynamically adding "%x"-style formatting switches, as well as   *
 * complete support for simply making separate builds.                      *
 *                                                                          *
 ****************************************************************************/

#ifndef MESSAGE_H
#define MESSAGE_H

#include <stdarg.h>
#include "../simulator/mctypes.h"

/****************************************************************************
 * Type of a function to handle a particular formatting switch.             *
 ****************************************************************************/
/* these functions should increment ioArgument as necessary. */
typedef void (*MSG_SwitchFunction)(char *inSwitch, WordPtr inStream, 
                                   va_list *ioArgument);


/****************************************************************************
 * Type of a block of messages, with associated constants.                  *
 ****************************************************************************/
typedef struct MSG_Msg
{
    int mIndex;			/* Index of this error message */
    int mPreChain;		/* Index of message to print before this one */
    char *mMessage;		/* The message itself */
    int mPostChain;		/* Index of message to print after this one */
    
    int mExnType;		/* if MSG_NO_EXN, MSG_Error() will return */
    unsigned int mExitStatus;	/* value to return with abort() */
} MSG_Msg;

typedef struct MSG_MessageBlock
{
    int mCount;			            /* No. of messages in this block */
    int mMinIndex, mMaxIndex;	    /* mMinIndex <= every index <= mMaxIndex */
    struct MSG_MessageBlock *mNext; /* Next block of messages in linked list */
    MSG_Msg *mMessages;		        /* Array of messages */
} MSG_MessageBlock;

/****************************************************************************
 * Initialization functions                                                 *
 ****************************************************************************/
void MSG_addSwitch(char inSwitch, MSG_SwitchFunction inFunction);
void MSG_addMessages(int inCount, MSG_Msg *inMessages);

/****************************************************************************
 * The routine that gets called to print a message, returning the exception *
 * type for the error message (mExnType)                                    *
 ****************************************************************************/
int MSG_vMessage(int inIndex, va_list *ap);

#endif /* MESSAGE_H */
