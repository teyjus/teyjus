#include <string.h>
#include "../dataformats.h"
#include "../io-datastructures.h"
#include "../mcstring.h"
#include "../mctypes.h"
#include "../../system/error.h"
#include "../abstmachine.h"


/*************************************************************************/
/* local tables assistanting term/type creation                          */
/*************************************************************************/
static DF_TermPtr *RT_freeVarTab;
static DF_TypePtr *RT_freeTypeVarTab;

static DF_TermPtr *RT_termQueueBeg, *RT_termQueueEnd;
static void RT_termQueueInit(DF_TermPtr *loc)
{
    RT_termQueueBeg = RT_termQueueEnd = loc;
}

static void RT_termQueueEnqueue(DF_TermPtr loc)
{
    *RT_termQueueEnd = loc;
    RT_termQueueEnd++;
}
static DF_TermPtr RT_termQueueDequeue()
{
    DF_TermPtr loc = *RT_termQueueBeg;
    RT_termQueueBeg++;
}


static DF_TypePtr *RT_typeQueueBeg, *RT_typeQueueEnd;
static void RT_typeQueueInit(DF_TypePtr *loc)
{
    RT_typeQueueBeg = RT_typeQueueEnd = loc;
}

static void RT_typeQueueEnqueue(DF_TypePtr loc)
{
    *RT_typeQueueEnd = loc;
    RT_typeQueueEnd++;
}

static DF_TypePtr RT_typeQueueDequeue()
{
    DF_TypePtr loc = *RT_typeQueueBeg;
    RT_typeQueueBeg++;
}


static DF_TermPtr termStartLoc;
static DF_TypePtr typeStartLoc;

/**************************************************************************/
/* Term/type creation functions invoked from OCaml Readterm module        */
/**************************************************************************/
/* initialize local free variable address table, free type variable address
   table, term address table and type address table; 
*/
void RT_initLocalTabs(int numFvs,int numTyFvs,int numTermArgs,int numTypeArgs)
{
    int     size   = numFvs + numTyFvs + numTermArgs + numTypeArgs;
    WordPtr tables = (WordPtr)EM_malloc(sizeof(DF_TermPtr) * size);

    //if (IO_freeVarTabFull(numFvs)) {} what to do?
    RT_freeVarTab  = (DF_TermPtr*)tables;
    RT_freeTypeVarTab = (DF_TypePtr*)(tables + numFvs);
    RT_termQueueInit((DF_TermPtr*)(RT_freeTypeVarTab + numTyFvs));
    RT_typeQueueInit((DF_TypePtr*)(RT_termQueueBeg + numTermArgs));

    RT_termQueueEnqueue(termStartLoc);
    RT_typeQueueEnqueue(typeStartLoc);
}

/* reclaim local tables */
void RT_cleanLocalTabs()
{
  free(RT_freeVarTab);
}

/* create a free variable: a fresh free variable is created on the current
   top of heap; it's address is entered into the local free variable table;
   its name and address are entered into the IO free variable table.
*/
void RT_buildFreeVar(char* name, int index) 
{
    int    length      = strlen(name);
    MemPtr strDataHead = AM_hreg;
    MemPtr strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    MemPtr fvTerm      = strData + MCSTR_numWords(length);
    MemPtr nhreg       = fvTerm + DF_TM_ATOMIC_SIZE;
    
    AM_heapError(nhreg);
    //create a machine string for the name of the free var
    DF_mkStrDataHead(strDataHead);
    MCSTR_toString((MCSTR_Str)strData, name, length);
    //create a free variable
    DF_mkVar(fvTerm, 0); //0 or current uc?
    AM_hreg = nhreg;
    
    //enter the address of the free variable into the local free var table
    RT_freeVarTab[index] = (DF_TermPtr)fvTerm;
    
    //enter the name and address of the free var into the IO free var table
    IO_enterFreeVarTab((DF_StrDataPtr)strDataHead, (DF_TermPtr)fvTerm);
}

/* create a free type variable on the top of heap and enter its address to 
   the local free type variable table.
*/
void RT_buildFreeTypeVar(int index)
{
    MemPtr nhreg = AM_hreg + DF_TY_ATOMIC_SIZE;

    AM_heapError(nhreg);     
    DF_mkFreeVarType(AM_hreg);   //create type variable
    RT_freeTypeVarTab[index] = (DF_TypePtr)AM_hreg; //enter local tyvar tab
    AM_hreg = nhreg;
}

/***************************************************************************/
/* Creating term/type on the heap:                                         */
/* If the term is:                                                         */
/* a) free variable:                                                       */
/*    a reference to the address in the ith entry (where i is the given    */
/*    index) of the local free variable table is created at the location   */
/*    dequeued from the term queue;                                        */
/* b) one with atomic size:                                                */
/*    the term is created at the location dequeued from the term queue; if */
/*    the term is an abstraction, then the address of its body is enqueued */
/* c) application:                                                         */
/*    the term is created at the current heap top; a reference to it is    */
/*    made at the location dequeued from the term queue; the addresses of  */
/*    the function and arguments are enqueued;                             */
/* b) constant with type association:                                      */
/*    the term is created at the current heap top; a reference to it is    */
/*    made at the location dequeued from the term queue; the addresses of  */
/*    its type environments are enqueued into the type queue.              */
/***************************************************************************/
void RT_buildIntTerm(int i)
{
    DF_mkInt((MemPtr)(RT_termQueueDequeue()), i);
}

void RT_buildRealTerm(double f)
{
    DF_mkFloat((MemPtr)(RT_termQueueDequeue()), f);
}

void RT_buildStringTerm(char* str)
{
    int    length      = strlen(str);
    MemPtr strDataHead = AM_hreg;
    MemPtr strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    MemPtr nhreg       = strData + MCSTR_numWords(length);

    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead); //create the string data on top of heap
    MCSTR_toString((MCSTR_Str)strData, str, length);
    AM_hreg = nhreg;  
    DF_mkStr((MemPtr)(RT_termQueueDequeue()), (DF_StrDataPtr)strDataHead);
}

void RT_buildNilTerm()
{
    DF_mkNil((MemPtr)(RT_termQueueDequeue()));
}

void RT_buildMConstantTerm(int index)
{
    DF_mkConst((MemPtr)(RT_termQueueDequeue()), AM_cstUnivCount(index), index);
}

void RT_buildPConstantTerm(int index, int typeEnvSize)
{
    MemPtr typeEnv = AM_hreg + DF_TM_TCONST_SIZE;
    MemPtr nhreg   = typeEnv + (DF_TY_ATOMIC_SIZE * typeEnvSize);
    
    AM_heapError(nhreg);
    DF_mkRef((MemPtr)(RT_termQueueDequeue()), (DF_TermPtr)AM_hreg);
    DF_mkTConst(AM_hreg, AM_cstUnivCount(index), index, (DF_TypePtr)typeEnv);
    AM_hreg = nhreg;
    
    while (typeEnvSize > 0) {
        RT_typeQueueEnqueue((DF_TypePtr)typeEnv);
        typeEnv += DF_TY_ATOMIC_SIZE;
        typeEnvSize--;
    }
}

void RT_buildFreeVarTerm(int index)
{
    DF_mkRef((MemPtr)(RT_termQueueDequeue()), RT_freeVarTab[index]);
}

void RT_buildDBTerm(int index)
{
    DF_mkBV((MemPtr)(RT_termQueueDequeue()), index);
}

void RT_buildAbstractionTerm(int numAbs)
{
    MemPtr nhreg = AM_hreg + DF_TM_ATOMIC_SIZE;
    AM_heapError(nhreg);
    DF_mkLam((MemPtr)(RT_termQueueDequeue()), numAbs, (DF_TermPtr)AM_hreg);
    RT_termQueueEnqueue((DF_TermPtr)AM_hreg);
    AM_hreg = nhreg;
}

void RT_buildConsTerm()
{
    MemPtr nhreg = AM_hreg + DF_TM_ATOMIC_SIZE * DF_CONS_ARITY;
    AM_heapError(nhreg);
    DF_mkCons((MemPtr)(RT_termQueueDequeue()), (DF_TermPtr)AM_hreg);
    
    RT_termQueueEnqueue((DF_TermPtr)AM_hreg);
    RT_termQueueEnqueue((DF_TermPtr)(AM_hreg + DF_TM_ATOMIC_SIZE));
    AM_hreg = nhreg;
}

void RT_buildApplicationTerm(int arity)
{
    MemPtr funcLoc = AM_hreg + DF_TM_APP_SIZE;
    MemPtr argsLoc = funcLoc + DF_TM_ATOMIC_SIZE;
    MemPtr nhreg   = argsLoc + DF_TM_ATOMIC_SIZE * arity;
    
    AM_heapError(nhreg);
    DF_mkApp(AM_hreg, arity, (DF_TermPtr)funcLoc, (DF_TermPtr)argsLoc);
    AM_hreg = nhreg;
    
    RT_termQueueEnqueue((DF_TermPtr)funcLoc);
    while (arity > 0) {
        RT_termQueueEnqueue((DF_TermPtr)argsLoc);
        argsLoc += DF_TM_ATOMIC_SIZE;
        arity--;
    }
}

/***************************************************************************/
/* Creating type on the heap:                                              */
/* If the type is:                                                         */
/* a) free variable:                                                       */
/*    a reference to the address in the ith entry (where i is the given    */
/*    index) of the local free ty variable table is created at the location*/
/*    dequeued from the type queue;                                        */
/* b) others:                                                              */
/*    a type is created at the location dequeued from the type queue with  */
/*    the argument vector on the current heap top; the addresses of        */
/*    arguments are enqueued.                                              */
/***************************************************************************/
void RT_buildArrowType()
{
    MemPtr nhreg = AM_hreg + DF_TY_ATOMIC_SIZE * DF_TY_ARROW_ARITY;
    AM_heapError(nhreg);
    DF_mkArrowType((MemPtr)(RT_typeQueueDequeue()), (DF_TypePtr)AM_hreg);
    RT_typeQueueEnqueue((DF_TypePtr)AM_hreg);
    RT_typeQueueEnqueue((DF_TypePtr)(AM_hreg + DF_TY_ATOMIC_SIZE));
    AM_hreg = nhreg;
}

void RT_buildSortType(int index)
{
    DF_mkSortType((MemPtr)(RT_typeQueueDequeue()), index);
}

void RT_buildStrType(int index, int arity)
{
    MemPtr argsLoc = AM_hreg + DF_TY_ATOMIC_SIZE;
    MemPtr nhreg   = argsLoc + DF_TY_ATOMIC_SIZE * arity;
    AM_heapError(nhreg);
    DF_mkStrFuncType(AM_hreg, index, arity);
    DF_mkStrType((MemPtr)(RT_typeQueueDequeue()), (DF_TypePtr)AM_hreg);
    AM_hreg = nhreg;
    
    while (arity > 0) {
        RT_typeQueueEnqueue((DF_TypePtr)argsLoc);
        argsLoc += DF_TY_ATOMIC_SIZE;
        arity--;
    }
}

void RT_buildFreeVarType(int index)
{
    DF_mkRefType((MemPtr)(RT_typeQueueDequeue()), RT_freeTypeVarTab[index]);
}
