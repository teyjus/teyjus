//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
/****************************************************************************
 *                                                                          *
 *   File message.c -- code to present messages to the user in Teyjus.      *
 * supports dynamically adding "%x"-style formatting switches, as well as   *
 * complete support for simply making separate builds.                      *
 *                                                                          *
 * future expansions: an Assert() facility, and a stack of error-cleanup    *
 *  functions to call in the case of an error.                              *
 *                                                                          *
 ****************************************************************************/
#include "message.h"
#include "stream.h"
#include "error.h"
#include "../simulator/mctypes.h"
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

/****************************************************************************
 * Prototypes of static functions used internally, as well as static data.  *
 ****************************************************************************/

/* NOTE: the characters for the standard switches, and their
   associated argument types, were taken from K&R p. 154 */
static void MSG_switchPrintfInt(char *inSwitch, WordPtr inStream,
                                va_list *ioArgument);
static void MSG_switchPrintfCharStar(char *inSwitch, WordPtr inStream,
                                     va_list *ioArgument);
static void MSG_switchPrintfDouble(char *inSwitch, WordPtr inStream,
                                   va_list *ioArgument);
static void MSG_switchPrintfVoidStar(char *inSwitch, WordPtr inStream,
                                     va_list *ioArgument);
static void MSG_switchPrintfPercent(char *inSwitch, WordPtr inStream,
                                    va_list *ioArgument);


typedef struct MSG_Switch
{
    char mSwitch;
    MSG_SwitchFunction mFunction;
} MSG_Switch;


#define MSG_MAX_DYNAMIC_SWITCHES 5 /* increase as necessary */
#define MSG_NUM_STANDARD_SWITCHES 15
#define MSG_MAX_SWITCHES (MSG_MAX_DYNAMIC_SWITCHES + MSG_NUM_STANDARD_SWITCHES)

static MSG_Switch MSG_switchFunctions[MSG_MAX_SWITCHES] =
{
   { 'd', MSG_switchPrintfInt },
   { 'i', MSG_switchPrintfInt },
   { 'o', MSG_switchPrintfInt },
   { 'x', MSG_switchPrintfInt },
   { 'X', MSG_switchPrintfInt },
   { 'u', MSG_switchPrintfInt },
   { 'c', MSG_switchPrintfInt },
   { 's', MSG_switchPrintfCharStar },
   { 'f', MSG_switchPrintfDouble },
   { 'e', MSG_switchPrintfDouble },
   { 'E', MSG_switchPrintfDouble },
   { 'g', MSG_switchPrintfDouble },
   { 'G', MSG_switchPrintfDouble },
   { 'p', MSG_switchPrintfVoidStar },
   { '%', MSG_switchPrintfPercent }
};

int MSG_numSwitches = MSG_NUM_STANDARD_SWITCHES;

static int MSG_doPrintMessageFlag = TRUE;
static char *MSG_storedMessage = NULL;
extern MSG_MessageBlock *MSG_messageBlockHead; /* defined in error.c */

/****************************************************************************
 * The private functions                                                    *
 ****************************************************************************/

static void MSG_switchPrintfInt(char *inSwitch, WordPtr inStream,
                                va_list *ioArgument)
{
    STREAM_printf(inStream, inSwitch, va_arg((*ioArgument), int));
}
static void MSG_switchPrintfCharStar(char *inSwitch, WordPtr inStream,
                                     va_list *ioArgument)
{
    STREAM_printf(inStream, inSwitch, va_arg((*ioArgument), char *));
}

static void MSG_switchPrintfDouble(char *inSwitch, WordPtr inStream,
                                   va_list *ioArgument)
{
    STREAM_printf(inStream, inSwitch, va_arg((*ioArgument), double));
}

static void MSG_switchPrintfVoidStar(char *inSwitch, WordPtr inStream,
                                     va_list *ioArgument)
{
    STREAM_printf(inStream, inSwitch, va_arg((*ioArgument), void *));
}

static void MSG_switchPrintfPercent(char *inSwitch, WordPtr inStream,
                                    va_list *ioArgument)
{
    STREAM_printf(inStream, "%%");
}


static char *MSG_extractSwitch(char *inSwitchString, char *outSwitchChar)
{
    static char lBuffer[32];
    char *lBufferPtr = lBuffer;

    *lBufferPtr++ = *inSwitchString++;	/* copy '%' */
    if (*inSwitchString == '-') *lBufferPtr++ = *inSwitchString++;
    while (isdigit(*inSwitchString))
        *lBufferPtr++ = *inSwitchString++;
    if (*inSwitchString == '.') {
        *lBufferPtr++ = *inSwitchString++;
        while (isdigit(*inSwitchString))
            *lBufferPtr++ = *inSwitchString++;
    }
    if (*inSwitchString == 'h' || *inSwitchString == 'l')
        *lBufferPtr++ = *inSwitchString++;
    /* the switch character is what we're pointing to now. */
    *outSwitchChar = *lBufferPtr++ = *inSwitchString;
    *lBufferPtr = '\0';

    /* we return a pointer to static space.  This function has very
       limited use, and this is OK. */
    return lBuffer;
}

static MSG_Msg *MSG_findMessage(int inIndex)
{
    MSG_MessageBlock *lMessageBlock = MSG_messageBlockHead;

    while (lMessageBlock) {
        /* if it might be in this block */
        if (inIndex >= lMessageBlock->mMinIndex &&
            inIndex <= lMessageBlock->mMaxIndex) {
            /* search within the block */
            MSG_Msg *lMessage = lMessageBlock->mMessages;
            int lCount = lMessageBlock->mCount;

            while (lCount--)
                if (lMessage->mIndex == inIndex) return lMessage;
                else lMessage++;
      }

      /* else move on to the next block */
      lMessageBlock = lMessageBlock->mNext;
   }
   return NULL;
}

static void MSG_printWithFormat(char *inString, WordPtr inStream, 
                                va_list *ioArgument)
{
    /* this could be (much much) faster, but will do for now */
    while (*inString) {
        if (*inString == '%') {
            char *lSwitchString;
            char lSwitchChar;
            int i;

            lSwitchString = MSG_extractSwitch(inString, &lSwitchChar);
            for (i = 0; i < MSG_numSwitches; i++)
                if (MSG_switchFunctions[i].mSwitch == lSwitchChar) {
                    MSG_switchFunctions[i].mFunction(lSwitchString,
                                                     inStream, ioArgument);
                    break;
                }
            inString += strlen(lSwitchString);
        } else STREAM_printf(inStream, "%c", *inString++);
    }
}

static void MSG_printMessage(MSG_Msg *inMessage, WordPtr inStream, 
                             va_list *ioArgument)
{
    /* print any messages which come before this one */
    if (inMessage->mPreChain != 0) {
        MSG_Msg *lPreMessage = MSG_findMessage(inMessage->mPreChain);
        if (lPreMessage) MSG_printMessage(lPreMessage, inStream, ioArgument);
    }

    /* parse the current message */
    MSG_printWithFormat(inMessage->mMessage, inStream, ioArgument);

    /* print any messages which come after this one */
    if (inMessage->mPostChain != 0) {
        MSG_Msg *lPostMessage = MSG_findMessage(inMessage->mPostChain);
        if (lPostMessage) MSG_printMessage(lPostMessage, inStream, ioArgument);
    }
}

/****************************************************************************
 * The public functions                                                     *
 ****************************************************************************/
void MSG_addSwitch(char inSwitch, MSG_SwitchFunction inFunction)
{
    if (MSG_numSwitches < MSG_MAX_SWITCHES) {
        MSG_Switch *lSwitch =
            &MSG_switchFunctions[MSG_numSwitches];

        lSwitch->mSwitch = inSwitch;
        lSwitch->mFunction = inFunction;

        MSG_numSwitches++;
    } else exit(1);			/* If we get here, increase
                               MSG_MAX_DYNAMIC_SWITCHES */
}


void MSG_addMessages(int inCount, MSG_Msg *inMessages)
{
    MSG_MessageBlock *lNewMessageBlock =
        (MSG_MessageBlock *)EM_malloc(sizeof(MSG_MessageBlock));

    lNewMessageBlock->mCount = inCount;
    lNewMessageBlock->mMessages = inMessages;

    lNewMessageBlock->mMinIndex = inMessages[0].mIndex;
    lNewMessageBlock->mMaxIndex = inMessages[0].mIndex;

    /* iterate through the supplied messages, finding the minimum and
       maximum indices.  Although this is a runtime expense that could
       be done manually in the source code, there's a chance it will be
       forgotten or mistake in the source code.  Since errors are not
       likely to occur too frequently, this might get overlooked.  A
       small initialization delay here ensures that these counts are
       correct. */
    while (inCount--) {
        if (lNewMessageBlock->mMinIndex > inMessages->mIndex)
            lNewMessageBlock->mMinIndex = inMessages->mIndex;
        if (lNewMessageBlock->mMaxIndex < inMessages->mIndex)
            lNewMessageBlock->mMaxIndex = inMessages->mIndex;
        
        inMessages++;
    }

    lNewMessageBlock->mNext = MSG_messageBlockHead;
    MSG_messageBlockHead = lNewMessageBlock;
}

int MSG_vMessage(int inIndex, va_list *ap)
{
    MSG_Msg *lMessage;

    /* get our arguments and print the message */
    lMessage = MSG_findMessage(inIndex);
    if (lMessage) {
        if (MSG_doPrintMessageFlag) {
            STREAM_flush(STREAM_stdout);
            MSG_printMessage(lMessage, STREAM_stderr, ap);
        } 
        /* GN June 23, 2012
       This code seems to do nothing that is observable---it creates a
       string in MSG_storedMessage that is not looked at till is freed
       by a use of this code again. Leaving it in for now, but should 
       be looked at in more detail and removed or actually used at that
       point. */
       else {
            WordPtr lStream = STREAM_toString();
            if (MSG_storedMessage) free(MSG_storedMessage);

            MSG_printMessage(lMessage, lStream, ap);

            MSG_storedMessage = strdup(STREAM_getString(lStream));
            STREAM_close(lStream);
        }
    } else exit(1);
    return lMessage->mExnType;
}
