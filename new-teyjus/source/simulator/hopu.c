/****************************************************************************/
/*                                                                          */
/*  File hopu.c. This file contains the main routines implementing the      */
/*  interpretive part of higher-order pattern unification.                  */
/*                                                                          */
/****************************************************************************/

#ifndef HOPU_C
#define HOPU_C

#include "hopu.h"
#include "mctypes.h"
#include "dataformats.h"
#include "hnorm.h"
#include "abstmachine.h"
#include "types.h"
#include "../system/error.h"  //to be changed
#include "../system/memory.h" //to be changed

//to be removed
#include <stdio.h>
#include "print.h"

/* Unify types associated with constants.                                 */
static void HOPU_typesUnify(DF_TypePtr tyEnv1, DF_TypePtr tyEnv2, int n)
{
    AM_pdlError(2*n);
    AM_initTypesPDL();
    TY_pushPairsToPDL((MemPtr)tyEnv1, (MemPtr)tyEnv2, n);
    TY_typesUnify();
}

/* Return the dereference of the abstraction body of the given term.       */
static DF_TermPtr HOPU_lamBody(DF_TermPtr tmPtr)
{
    tmPtr = DF_termDeref(tmPtr);
    while (DF_isLam(tmPtr)) tmPtr = DF_termDeref(DF_lamBody(tmPtr));
    return tmPtr;
}

/* Eta expands a rigid term whose term pointer and decomposition are given */
/* by arguments. The new lambda body is returned. (It is unnecessary to    */
/* create a new lambda term for the abstractions in the front of the eta   */
/* expanded form. Note that the term head and argument vector are updated  */
/* as side-effect.                                                         */
static DF_TermPtr HOPU_etaExpand(DF_TermPtr *h, DF_TermPtr *args, int nargs, 
                                 int nabs)
{
    DF_TermPtr hPtr = *h, oldArgs = *args, rtPtr;
    MemPtr     suspLoc;  //where susps are to be created
    int        newArity = nargs + nabs;
    if (DF_isBV(hPtr)){  //lift index by nabs if the head is a bound variable
        int ind = DF_bvIndex(hPtr) + nabs;
        AM_embedError(ind);
        AM_heapError(AM_hreg + DF_TM_ATOMIC_SIZE);
        *h = hPtr =(DF_TermPtr)AM_hreg; //update head pointer
        DF_mkBV(AM_hreg,ind);
        AM_hreg += DF_TM_ATOMIC_SIZE;
    }
    AM_arityError(newArity);
    AM_heapError(AM_hreg + nargs * DF_TM_SUSP_SIZE + newArity*DF_TM_ATOMIC_SIZE
                 + DF_TM_APP_SIZE);
    suspLoc = AM_hreg;
    AM_hreg += nargs * DF_TM_SUSP_SIZE;  //allocate space for nargs suspensions
    rtPtr   = (DF_TermPtr)AM_hreg;       //new application
    DF_mkApp(AM_hreg, newArity, hPtr, (DF_TermPtr)(AM_hreg + DF_TM_APP_SIZE));
    AM_hreg += DF_TM_APP_SIZE;
    *args = (DF_TermPtr)AM_hreg;         //update arg vector pointer
    for (; nargs > 0; nargs--){//create suspensions over original arguments 
        DF_mkSusp(suspLoc, 0, nabs, DF_termDeref(oldArgs), DF_EMPTY_ENV);
        DF_mkRef(AM_hreg, (DF_TermPtr)suspLoc);
        suspLoc += DF_TM_SUSP_SIZE; AM_hreg += DF_TM_ATOMIC_SIZE; 
        oldArgs = (DF_TermPtr)(((MemPtr)oldArgs) + DF_TM_ATOMIC_SIZE);
    }
    for (; nabs > 0; nabs--){//create de Bruijn indices from #nabs to #1
        DF_mkBV(AM_hreg, nabs); 
        AM_hreg += DF_TM_ATOMIC_SIZE;
    }
    return rtPtr;
}

/***************************************************************************/
/*                    PATTERN RECOGNITION                                  */
/*                                                                         */
/* Auxiliary functions for recognizing LLambda pattens for flexible terms. */
/***************************************************************************/
/* Whether a bound variable occurs in the given arguments.                 */
/* It is assumned that the given arguments can only contain bound variables*/
/* and constants.                                                          */
static Boolean HOPU_uniqueBV(int bvInd, DF_TermPtr args, int n)
{
    DF_TermPtr tPtr;
    for ( ;  n > 0 ; n-- ){
        tPtr = DF_termDeref(args);
        if (DF_isBV(tPtr) && (bvInd == DF_bvIndex(tPtr))) return FALSE;
        //otherwise different bv or constant, check the next
        args = (DF_TermPtr)(((MemPtr)args) + DF_TM_ATOMIC_SIZE);
    }
    return TRUE;
}

/* Whether a constant occurs in the given arguments.                       */
/* It is assumned that the given arguments can only contain bound variables*/
/* and constants.                                                          */
/* CHANGES have to be made here if the semantics of local constants are    */
/* changed with respect to polymorphism.                                   */
static Boolean HOPU_uniqueConst(DF_TermPtr cPtr, DF_TermPtr args, int n)
{
    DF_TermPtr tPtr;
    for ( ; n > 0 ; n--){
        tPtr = DF_termDeref(args);
        if (DF_isConst(tPtr) && DF_sameConsts(tPtr, cPtr)) {
            if (DF_isTConst(tPtr)) {
                EM_try { 
                    HOPU_typesUnify(DF_constType(tPtr), DF_constType(cPtr), 
                                    AM_cstTyEnvSize(DF_constTabIndex(cPtr)));
                } EM_catch {
                    if (EM_CurrentExnType == EM_TY_UNI_FAIL) {
                        AM_resetTypesPDL();//remove tys added for ty unif
                    } else EM_reThrow();
                }
            } else  return FALSE; 
        } //otherwise different constant or bv, check the next
        args = (DF_TermPtr)(((MemPtr)args) + DF_TM_ATOMIC_SIZE);
    } //for loop
    return TRUE;
}

/* Checking whether the argments of the head normal form given by registers*/
/* AM_argVec, AM_numArgs and AM_numAbs are those of an eta-expanded form.  */
/* Specifically, the arguments are attempted to match de Bruijn indices    */
/* #n ... #1, where n is the current value of AM_numAbs.                   */ 
/* It is assumed that the argument vector is not empty.                    */
static Boolean HOPU_isEtaExpArgs()
{
    if (AM_numArgs != AM_numAbs) return FALSE;
    else {
        int        i      = AM_numAbs;
        Boolean    match  = TRUE;
        DF_TermPtr oneArg = AM_argVec;
        DF_TermPtr head   = AM_head;
        while (match && (i > 0)){
            HN_hnorm(oneArg);
            if (AM_numArgs == 0) 
                match = ((AM_numArgs == 0) && DF_isBV(AM_head) &&
                         (DF_bvIndex(AM_head) == i));
            else 
                match = (DF_isBV(AM_head) && (DF_bvIndex(AM_head)-AM_numAbs==i)
                         && HOPU_isEtaExpArgs());
            oneArg = (DF_TermPtr)(((MemPtr)oneArg + DF_TM_ATOMIC_SIZE));
            i--;
        }
        AM_head = head;
        return match;
    }
}

/* Checking whether the arguments of a flexible term satisfy with the      */
/* LLambda pattern with respect to the universe count of its flex head.    */
/* CHANGES have to be made here if the semantics of local constants are    */
/* changed with respect to polymorphism.                                   */
static Boolean HOPU_isLLambda(int uc, int nargs, DF_TermPtr args)
{
    if (nargs == 0) return TRUE;
    else {
        int i;
        DF_TermPtr myArgs = args;
        for (i = 0; i < nargs; i++){
            HN_hnorm(args);
            if (AM_numArgs == 0) {
                if (AM_numAbs != 0) return FALSE; //abstraction
                if (DF_isBV(AM_head)) {           //bound variable
                    if (!HOPU_uniqueBV(DF_bvIndex(AM_head), myArgs, i))
                        return FALSE;
                } else if (DF_isConst(AM_head)) { //constant
                    if (!(uc < DF_constUnivCount(AM_head) && 
                          HOPU_uniqueConst(AM_head, myArgs, i))) return FALSE;
                } else return FALSE;              //other sort of terms
            } else { //AM_numArgs != 0
                if (DF_isBV(AM_head)) {           //bound variable head
                    int dbInd = DF_bvIndex(AM_head) - AM_numAbs; //eta-norm
                    if (dbInd > 0 && HOPU_uniqueBV(dbInd, myArgs, i) &&
                        HOPU_isEtaExpArgs()) {
                        //trail(args);
                        DF_mkBV((MemPtr)args, dbInd);
                    } else return FALSE;
                } else { //!(DF_isBV(AM_head))
                    if (DF_isConst(AM_head)) {    //constant head
                        if (uc < DF_constUnivCount(AM_head) && 
                            HOPU_uniqueConst(AM_head, myArgs, i) &&
                            HOPU_isEtaExpArgs()) {
                            //trail(args);
                            if (DF_isTConst(AM_head)) 
                                DF_mkRef((MemPtr)args, AM_head);
                            else DF_copyAtomic(AM_head, (MemPtr)args);
                        } else return FALSE;
                    } else return FALSE;          //other sort of terms
                }       //!(DF_isBV(AM_head))
            }        //AM_numArgs != 0
            args = (DF_TermPtr)(((MemPtr)args) + DF_TM_ATOMIC_SIZE);
        } //for loop
        return TRUE;
    } //nargs != 0
}


/***************************************************************************/
/*                            BINDING                                      */
/*                                                                         */
/* Attempt to find bindings for free variables (counter part of mksubst in */
/* the sml pattern unfication code).                                       */
/***************************************************************************/
/* A flag denoting whether new structure is created during the process of  */
/* finding substitutions.                                                  */
static Boolean HOPU_copyFlagGlb = FALSE;

/* Return a non-zero index of a bound variable appears in a list of        */
/* arguments. Note the index is the position from the right and the        */
/* embedding level is taken into account.                                  */
static int HOPU_bvIndex(int dbInd, DF_TermPtr args, int nargs, int lev)
{
    int ind;
    dbInd -= lev;
    for (ind = nargs; ind > 0; ind--){
        DF_TermPtr tPtr = DF_termDeref(args);
        if (DF_isBV(tPtr) && (dbInd == DF_bvIndex(tPtr))) return (ind+lev);
        //otherwise try the next
        args = (DF_TermPtr)(((MemPtr)args) + DF_TM_ATOMIC_SIZE);
    }
    return 0; //not found
}

/* Return a non-zero index if a constant appears in a list of arguments.   */
/* Note the index is the position from the right and the embedding level   */
/* is taken into account.                                                  */
/* CHANGES have to be made here if the semantics of local constants are    */
/* changed with respect to polymorphism.                                   */
static int HOPU_constIndex(DF_TermPtr cPtr, DF_TermPtr args, int nargs, int lev)
{
    int ind;
    for (ind = nargs; ind > 0; ind--){
        DF_TermPtr tPtr = DF_termDeref(args);
        if (DF_isConst(tPtr) && DF_sameConsts(tPtr, cPtr)) {
               if (DF_isTConst(tPtr)) {
                EM_try { 
                    HOPU_typesUnify(DF_constType(tPtr), DF_constType(cPtr), 
                                    AM_cstTyEnvSize(DF_constTabIndex(cPtr)));
                    return (ind+lev);
                } EM_catch {//remove types added for ty unif from the PDL
                    if (EM_CurrentExnType == EM_TY_UNI_FAIL) AM_resetTypesPDL();
                    else EM_reThrow();
                }
               } else return (ind+lev);  //cPtr does not have type associated
        } //otherwise try the next
        args = (DF_TermPtr)(((MemPtr)args) + DF_TM_ATOMIC_SIZE);
    }
    return 0; //not found
}

/***************************************************************************/
/*                 BINDING FOR FLEX-FLEX                                   */
/*                                                                         */
/* Auxiliary functions for solving flex-flex pairs.                        */
/* Non-LLambda pairs are delayed onto the disagreement list.               */
/***************************************************************************/

/* Collect raising components for internal variable in the LLambda case    */
/* when it is known it has a higher universe index than the outside llambda*/
/* variable.                                                               */
/* It is assumned that the incoming argument vector has a size larger than */
/* zero.                                                                   */
/* As a result of this process, segments of the argument vectors for both  */
/* variables are decided. That for the internal variable is created on the */
/* current top of heap, while that for the outside variable, each          */
/* argument of which must be a de Bruijn index, is recorded into an integer*/
/* array which is set by side-effect.                                      */
/* The number returned by this procedure is the length of both of the      */
/* argument vector segements. Raising occured when this number is non-zero.*/
/* CHANGES have to be made here if the semantics of local constants are    */
/* changed with respect to polymorphism.                                   */
static int HOPU_raise(int varuc, DF_TermPtr args, int nargs, int emblev, 
                      int *args11)
{
    int numRaised = 0;   //number of args that have been raised
    AM_heapError(AM_hreg + nargs * DF_TM_ATOMIC_SIZE);//max possible size
    for (; nargs > 0; nargs--){
        DF_TermPtr tmPtr = DF_termDeref(args);
        if (DF_isConst(tmPtr) && (DF_constUnivCount(tmPtr) <= varuc)){
            args11[numRaised] = nargs + emblev;               //args11        
            if (DF_isTConst(tmPtr)) DF_mkRef(AM_hreg, tmPtr); //args21
            else DF_copyAtomic(tmPtr, AM_hreg);
            AM_hreg += DF_TM_ATOMIC_SIZE;
            numRaised++;
        }
        args = (DF_TermPtr)(((MemPtr)args) + DF_TM_ATOMIC_SIZE);
    }        
    return numRaised;
} 


/* Generate the indices for items not to be pruned when the internal       */
/* variable is known to have a universe index greater than that of the     */
/* external one.                                                           */
/* It is assumned that arg vector of the internal flex term has a size     */
/* larger than  zero.                                                      */
/* As a result of this process, segments of the argument vectors for both  */
/* variables are decided. That for the internal variable is created on the */
/* current top of heap, while that for the outside variable, each          */
/* argument of which must be a de Bruijn index, is recorded into an integer*/
/* array which is set by side-effect.                                      */
/* The number returned by this procedure is the length of both of the      */
/* argument vector segements. Pruning occured when this number is smaller  */
/* than the size of the arg vector of the internal term.                   */
static int HOPU_prune(DF_TermPtr args1, int nargs1, DF_TermPtr args2, 
                      int nargs2, int emblev, int *args12)
{

    int numNotPruned = 0;
    AM_heapError(AM_hreg + nargs2 * DF_TM_ATOMIC_SIZE);//max possible size
    for (; nargs2 > 0; nargs2--){
        DF_TermPtr tmPtr = DF_termDeref(args2);
        if (DF_isConst(tmPtr)) {
            int ind = HOPU_constIndex(tmPtr, args1, nargs1, emblev);
            if (ind > 0) { 
                args12[numNotPruned] = ind;                   //args12 
                DF_mkBV(AM_hreg, nargs2);                     //args22
                AM_hreg += DF_TM_ATOMIC_SIZE;
                numNotPruned ++;
                HOPU_copyFlagGlb = TRUE;
            } //ind == 0 the argument is pruned 
        } else {//bv
            int ind = DF_bvIndex(tmPtr);
            if (ind > emblev) {
                int newind = HOPU_bvIndex(ind, args1, nargs1, emblev);
                if (newind > 0) {
                    args12[numNotPruned] = newind;            //args12
                    DF_mkBV(AM_hreg, nargs2);                 //args22
                    AM_hreg += DF_TM_ATOMIC_SIZE;
                    numNotPruned ++;
                    if (ind != newind) HOPU_copyFlagGlb = TRUE; 
                } //newind == 0 the argument is pruned
            } else {//ind <= lev
                args12[numNotPruned] = ind;                   //args12
                DF_mkBV(AM_hreg, nargs2);                     //args22
                AM_hreg += DF_TM_ATOMIC_SIZE;
                numNotPruned ++;
            }
        }       //bv
        args2 = (DF_TermPtr)(((MemPtr)args2) + DF_TM_ATOMIC_SIZE);
    } //for loop
    return numNotPruned;
} 

/* When the index of the internal variable is less than or equal to that   */ 
/* of the external one in the LLambda case, we have to raise the outside   */
/* variable over those constants in the internal list that have smaller    */
/* index and we have to prune other constants and bound variables in this  */
/* list that are not shared.                                               */
/* It is assumned that the arg vector of the internal flex term has a size */
/* larger than zero.                                                       */
/* As a result of this process, the argument vectors for both  variables   */
/* are decided. That for the outside variable is created on the current    */
/* top of heap, while that for the internal variable, each argument of     */
/* which must be a de Bruijn index, is recorded into an integer array which*/
/* is set by side-effect.                                                  */
/* The number returned by this procedure is the length of both of the      */
/* argument vectors. Pruning occured when this number is smaller than the  */
/* size of the arg vector of the internal term.                            */
/* CHANGES have to be made here if the semantics of local constants are    */
/* changed with respect to polymorphism.                                   */
static int HOPU_pruneAndRaise(DF_TermPtr args1, int nargs1, DF_TermPtr args2,
                              int nargs2, int emblev, int *args)
{
    int numNotPruned = 0;    
    AM_heapError(AM_hreg + nargs2 * DF_TM_ATOMIC_SIZE); //max possible size
    for (; nargs2 > 0; nargs2 --){
        DF_TermPtr tmPtr = DF_termDeref(args2);
        if (DF_isBV(tmPtr)){
            int ind = DF_bvIndex(tmPtr);
            if (ind > emblev) {
                int newind = HOPU_bvIndex(ind, args1, nargs1, emblev);
                if (newind > 0) {
                    DF_mkBV(AM_hreg, newind);         //args for outside var
                    AM_hreg += DF_TM_ATOMIC_SIZE;
                    args[numNotPruned] = nargs2;      //args for internal var 
                    numNotPruned ++;
                    if (ind != newind) HOPU_copyFlagGlb = TRUE;
                } // newind == 0, the argument is prubed
            } else { //ind <= emblev
                DF_mkBV(AM_hreg, ind);                //args for outside var
                AM_hreg += DF_TM_ATOMIC_SIZE;
                args[numNotPruned] = nargs2;          //args for internal var
                numNotPruned ++;
            }
        } else { //tmPtr is const
            if (DF_constUnivCount(tmPtr) > AM_adjreg){                
                int ind = HOPU_constIndex(tmPtr, args1, nargs1, emblev); 
                if (ind > 0) {
                    DF_mkBV(AM_hreg, ind);            //args for outside var
                    AM_hreg += DF_TM_ATOMIC_SIZE;
                    args[numNotPruned] = nargs2;      //args for internal var
                    numNotPruned ++;
                    HOPU_copyFlagGlb = TRUE;
                } //else ind = 0, the argument is pruned  
            } else { //const uc <= AM_adjreg
                if (DF_isTConst(tmPtr)) DF_mkRef(AM_hreg, tmPtr);//args out var
                else DF_copyAtomic(tmPtr, AM_hreg);
                AM_hreg += DF_TM_ATOMIC_SIZE;
                args[numNotPruned] = nargs2;          //args for internal var
                numNotPruned ++;
            }
        }
        args2 = (DF_TermPtr)(((MemPtr)args2) + DF_TM_ATOMIC_SIZE);
    } //for loop
    return numNotPruned;
} 

/* Generating the arguments of a pruning substitution for the case when    */
/* when trying to unify two flexible terms of the form                     */
/*             (F a1 ... an) = lam(k, (F b1 ... bm))                       */
/* The resulted argument vector is created on the current top of heap, and */
/* the integer returned by this procedure is the length of the argument    */
/* vector resulted from pruning. Pruning takes place if this value is      */
/* smaller that nargs2.                                                    */
/* It is assumed that the sum of n and k is the same as m.                 */
/* CHANGES have to be made here if the semantics of local constants are    */
/* changed with respect to polymorphism.                                   */
static int HOPU_pruneSameVar(DF_TermPtr args1, int nargs1, DF_TermPtr args2,
                             int nargs2, int lev)
{
    if (nargs2 == 0) return 0;
    else {
        int numNotPruned = 0;
        DF_TermPtr tPtr2;
        AM_heapError(AM_hreg + nargs2 * DF_TM_ATOMIC_SIZE); //max possible size
        nargs1 = nargs2 - nargs1; //reused nargs1
        for (; nargs2 > nargs1; nargs2 --){
            DF_TermPtr tPtr1 = DF_termDeref(args1);
            tPtr2 = DF_termDeref(args2);
            if (DF_isBV(tPtr1)){
                int ind = DF_bvIndex(tPtr1) + lev;
                if (DF_isBV(tPtr2) && (ind == DF_bvIndex(tPtr2))){
                    DF_mkBV(AM_hreg, nargs2); AM_hreg += DF_TM_ATOMIC_SIZE;
                    numNotPruned++;
                    if (nargs2 != ind) HOPU_copyFlagGlb = TRUE;
                } //else this argument is pruned
            } else {// tPtr1 is a constant
                if (DF_isConst(tPtr2) && DF_sameConsts(tPtr1, tPtr2)){
                    if (DF_isTConst(tPtr2)) {
                        EM_try {
                        HOPU_typesUnify(DF_constType(tPtr1),DF_constType(tPtr2),
                                     AM_cstTyEnvSize(DF_constTabIndex(tPtr1)));
                        DF_mkBV(AM_hreg, nargs2); AM_hreg += DF_TM_ATOMIC_SIZE;
                        numNotPruned++;
                        HOPU_copyFlagGlb = TRUE;
                        } EM_catch {//remove tys for type unif from the PDL
                            if (EM_CurrentExnType == EM_TY_UNI_FAIL) 
                                AM_resetTypesPDL();        
                            else EM_reThrow();
                        } //EM_catch
                    } else {//no type association
                        DF_mkBV(AM_hreg, nargs2); AM_hreg+=DF_TM_ATOMIC_SIZE;
                        numNotPruned++;
                        HOPU_copyFlagGlb = TRUE;
                    }
                }//else pruned 
            }       //tPtr1 is a constant
            args1 = (DF_TermPtr)(((MemPtr)args1) + DF_TM_ATOMIC_SIZE);
            args2 = (DF_TermPtr)(((MemPtr)args2) + DF_TM_ATOMIC_SIZE);
        } //for (; nargs2 > nargs1; nargs2--)
        for (; nargs2 > 0; nargs2--){     
            tPtr2 = DF_termDeref(args2);
            if (DF_isBV(tPtr2) && (DF_bvIndex(tPtr2) == nargs2)){
                DF_mkBV(AM_hreg, nargs2); AM_hreg += DF_TM_ATOMIC_SIZE;
                numNotPruned++;
            } //else pruned
            args2 = (DF_TermPtr)(((MemPtr)args2) + DF_TM_ATOMIC_SIZE);
        } //for (; nargs2 > 0; nargs2--)
        return numNotPruned;
    } //nargs2 != 0
}

/* Push a new free variable with given universe count onto the current heap */  
/* top.                                                                     */
static void HOPU_pushVarToHeap(int uc)
{
    MemPtr newhtop = AM_hreg + DF_TM_ATOMIC_SIZE;
    AM_heapError(newhtop);
    DF_mkVar(AM_hreg, uc);
    AM_hreg = newhtop;
}

/* Perform substitution to realize pruning and raising for an internal     */
/* variable in the LLambda situation when the variable has an index greater*/
/* than that of the outside one                                            */
/* This procedure is also used to perform substitution for flex-flex pairs */
/* with same variable heads in the LLambda situation.                      */
static void HOPU_mkPandRSubst(DF_TermPtr hPtr, DF_TermPtr args, int nargs,
                              DF_TermPtr vPtr, int nabs)
{
    //trail(vPtr)
    if (nargs == 0) {
        if (nabs == 0) DF_mkRef((MemPtr)vPtr, hPtr);
        else DF_mkLam((MemPtr)vPtr, nabs, hPtr);
    } else { //nargs > 0
        DF_TermPtr tPtr = (DF_TermPtr)AM_hreg;
        AM_heapError(AM_hreg + DF_TM_APP_SIZE);
        AM_arityError(nargs);
        DF_mkApp(AM_hreg, nargs, hPtr, args);  //application body
        AM_hreg += DF_TM_APP_SIZE;
        AM_embedError(nabs);
        if (nabs == 0) DF_mkRef((MemPtr)vPtr, tPtr);
        else DF_mkLam((MemPtr)vPtr, nabs, tPtr);
    }
}

/* Perform substitution to realize pruning and raising for an internal     */
/* variable in the LLambda situation when the variable has an index smaller*/
/* than or equal to that of the outside one                                */
/* The arguments of the substitution which should be de Bruijn indices     */ 
/* are given by an integer array.                                          */
static void HOPU_mkPrunedSubst(DF_TermPtr hPtr, int *args, int nargs, 
                               DF_TermPtr vPtr, int nabs)
{
    //trail(vPtr)
    if (nargs == 0) {
        if (nabs == 0) DF_mkRef((MemPtr)vPtr, hPtr);
        else DF_mkLam((MemPtr)vPtr, nabs, hPtr);
    } else { //nargs > 0;
        DF_TermPtr argvec = (DF_TermPtr)AM_hreg, appPtr;
        int i;
        AM_heapError(AM_hreg + DF_TM_APP_SIZE + nargs * DF_TM_ATOMIC_SIZE);
        for (i = 0; i < nargs; i++){//commit bvs in args onto heap
            DF_mkBV(AM_hreg, args[i]);
            AM_hreg += DF_TM_ATOMIC_SIZE;
        }
        appPtr = (DF_TermPtr)AM_hreg;
        DF_mkApp(AM_hreg, nargs, hPtr, argvec);
        AM_hreg += DF_TM_APP_SIZE;
        if (nabs == 0) DF_mkRef((MemPtr)vPtr, appPtr);
        else DF_mkLam((MemPtr)vPtr, nabs, appPtr);
    }
}

/* Generating the partial structure of a substitution to realize pruning   */ 
/* and raising for an outside variable in the LLambda situation when the   */
/* variable has an index smaller than that of the internal one.            */
/* The arguments of the susbsitution consists of two segments of de Burijn */
/* indices, which are given by two integer arrays respectively.            */
static DF_TermPtr HOPU_mkPandRTerm(DF_TermPtr hPtr, int args1[], int nargs1,
                                   int args2[], int nargs2)
{
    if ((nargs1 == 0) && (nargs2 == 0)) return hPtr;
    else {
        DF_TermPtr args = (DF_TermPtr)AM_hreg, rtPtr;
        int nargs = nargs1 + nargs2;   //new arity (non-zero)
        int i;
        AM_arityError(nargs);
        AM_heapError(AM_hreg + DF_TM_APP_SIZE + nargs * DF_TM_ATOMIC_SIZE);
        for (i = 0; i < nargs1 ; i++){ //commit bvs in a11 onto heap 
            DF_mkBV(AM_hreg, args1[i]);
            AM_hreg += DF_TM_ATOMIC_SIZE;
        }
        for (i = 0; i < nargs2 ; i++){ //commit bvs in a12 onto heap
            DF_mkBV(AM_hreg, args2[i]);
            AM_hreg += DF_TM_ATOMIC_SIZE;
        }
        rtPtr = (DF_TermPtr)AM_hreg;
        DF_mkApp(AM_hreg, nargs, hPtr, args);
        AM_hreg += DF_TM_APP_SIZE;
        return rtPtr;
    }
}

/* Generating the partial structure of a substitution to realize pruning   */ 
/* and raising for an internal variable in the LLambda situation when the  */
/* variable has an index greater than or equal to that of the outside one. */
static DF_TermPtr HOPU_mkPrunedTerm(DF_TermPtr hPtr, DF_TermPtr args, int nargs)
{
    if (nargs == 0) return hPtr;
    else {
        DF_TermPtr rtPtr = (DF_TermPtr)AM_hreg;
        AM_heapError(AM_hreg + DF_TM_APP_SIZE);
        DF_mkApp(AM_hreg, nargs, hPtr, args);
        AM_hreg += DF_TM_APP_SIZE;
        return rtPtr;
    }
}

/* Find the (partial) structure of the substitution for a flex head of a      */
/* LLambda term corresponding to an internal flex term which is known to be   */
/* LLambda. The internal free variable is bound to a proper substitution  as  */
/* side-effect.
/* The arguments of this procedure are:                                       */
/*  args1 : reference to the argument vector of the outside flex term         */
/*  nargs1: number of arguments of the outside flex term                      */
/*  uc    : universe count of the internal free variable                      */
/*  tPtr2 : refers to the dereference of ABSTRACTION BODY of the internal     */
/*          flex term                                                         */
/*  fhPtr : refers to the head of the internal flex term                      */
/*  args2 : refers to the argument vector of the internal flex term           */
/*  nargs2: number of arguments of the internal flex term                     */
/*  lev   : the abstraction context of the internal flex term                 */
/* Note that the outside free variable and its universe count are assumed to  */
/* be given by the global variables (registers) AM_vbbreg and AM_adjreg.      */
static DF_TermPtr HOPU_flexNestedLLambda(DF_TermPtr args1, int nargs1, int uc,
                   DF_TermPtr tPtr2, DF_TermPtr fhPtr, DF_TermPtr args2, 
                   int nargs2, int lev)
{
    DF_TermPtr bnd;  //(partial) binding for the outside free var 
    MemPtr     oldhtop = AM_hreg;
    DF_TermPtr heapArgs = (DF_TermPtr)AM_hreg;
    if (AM_adjreg < uc){
        int *args11 = NULL, *args12 = NULL; //hold args of bnd of the outside v
        int nargs11 = 0,    nargs12 = 0;
        if (nargs1 != 0) {
            args11 = (int*)EM_malloc(nargs1 * sizeof(int));
            nargs11 = HOPU_raise(uc, args1, nargs1, lev, args11);
        }
        if (nargs2 != 0) {
            args12 = (int*)EM_malloc(nargs2 * sizeof(int));
            nargs12 = HOPU_prune(args1, nargs1, args2, nargs2, lev, args12); 
        }
        if ((nargs11 == 0) && (nargs12 == nargs2)) {//neither raised nor pruned
            AM_hreg = oldhtop; //the internal free var remains unbound          
            //trail(fhPtr);
            DF_modVarUC(fhPtr, AM_adjreg);
            if (HOPU_copyFlagGlb)
                bnd = HOPU_mkPandRTerm(fhPtr, args11, nargs11, args12, nargs12);
            else bnd = tPtr2;
        } else {                                    //raised or pruned
            DF_TermPtr newVar = (DF_TermPtr)AM_hreg;
            HOPU_pushVarToHeap(AM_adjreg);
            HOPU_mkPandRSubst(newVar, heapArgs, nargs11+nargs12, fhPtr, nargs2);
            bnd = HOPU_mkPandRTerm(newVar, args11, nargs11, args12, nargs12);
            HOPU_copyFlagGlb = TRUE;
        }
        if (nargs1 != 0) free(args11); if (nargs2 != 0) free(args12);        
    } else { //AM_adjreg >= uc
        int *newargs2 = NULL;
        int nnewargs2 = 0;
        if (nargs2 != 0) {
            newargs2 = (int*)EM_malloc(nargs2 * sizeof(int));
            nnewargs2 = HOPU_pruneAndRaise(args1,nargs1,args2,nargs2,lev,
                                           newargs2);
        }
        if (nnewargs2 == nargs2){//not pruned
            if (HOPU_copyFlagGlb)
                bnd = HOPU_mkPrunedTerm(fhPtr, heapArgs, nnewargs2);
            else { AM_hreg = oldhtop; bnd = tPtr2; }
        } else {                 //pruned            
            DF_TermPtr newVar = (DF_TermPtr)AM_hreg;
            HOPU_pushVarToHeap(uc);
            HOPU_mkPrunedSubst(newVar, newargs2, nnewargs2, fhPtr, nargs2);
            bnd = HOPU_mkPrunedTerm(newVar, heapArgs, nnewargs2);
            HOPU_copyFlagGlb = TRUE;
        }
        if (nargs2 != 0) free(newargs2);
    }        //AM_adjreg >= uc
    return bnd;
}

/* Checking the arguments of a flex (non-LLambda) term to see whetehr a    */
/* free variable same as that currently in the AM_vbbreg register, a free  */
/* variable with higher univ count than that currently in the AM_adjreg    */
/* register, a constant with higher univ count than that currently in      */
/* AM_adjreg, or a de Bruijn index bound by abstractions over the variable */
/* for which a substitution is being constructed occurs.                   */
/* If one of the situations occurs, exception is raised.                   */   
static void HOPU_flexCheck(DF_TermPtr args, int nargs, int emblev)
{
    for (; nargs > 0; nargs --){
        int nemblev;  
        HN_hnorm(args);
        nemblev = emblev + AM_numAbs;
        if (AM_rigFlag){
            if (DF_isBV(AM_head)) {
                if (DF_bvIndex(AM_head) > nemblev) EM_throw(EM_HOPU_FAIL);
            } else {
                if (DF_isConst(AM_head)&&(DF_constUnivCount(AM_head)>AM_adjreg))
                    EM_throw(EM_HOPU_FAIL);
            } //otherwise succeeds
        } else { //AM_rigFlag == FALSE
            if ((AM_vbbreg == AM_head) || (DF_fvUnivCount(AM_head)>AM_adjreg))
                EM_throw(EM_HOPU_FAIL);
        }
        HOPU_flexCheck(AM_argVec, AM_numArgs, nemblev);
        args = (DF_TermPtr)(((MemPtr)args) + DF_TM_ATOMIC_SIZE);
    }
}

/* Generating a term on the top of heap which is to be added into a        */
/* disagreement pair.                                                      */
/* The term has the following structure:                                   */
/* (h [|a1, 0, lev, nil|] ... [|an, 0, lev, nil|] #lev ... #1)             */
/* It is assumed that nargs and lev are not equal to zero.                 */
static void HOPU_mkTermNLL(DF_TermPtr h, DF_TermPtr args, int nargs, int lev)
{
    int newArity = nargs + lev;
    MemPtr newArgs = AM_hreg + DF_TM_APP_SIZE; //spare app (head) size on heap
    AM_arityError(newArity);
    AM_heapError(AM_hreg + nargs*DF_TM_SUSP_SIZE + newArity*DF_TM_ATOMIC_SIZE
                 + DF_TM_APP_SIZE);
    DF_mkApp(AM_hreg, newArity, h, (DF_TermPtr)newArgs);
    AM_hreg += (DF_TM_APP_SIZE + newArity * DF_TM_ATOMIC_SIZE);//alloc arg vec
    for (; nargs > 0; nargs--){ //[|ai, 0, lev, nil|], for i <= nargs
        DF_mkRef(newArgs, (DF_TermPtr)AM_hreg); 
        DF_mkSusp(AM_hreg, 0, lev, DF_termDeref(args), DF_EMPTY_ENV);
        newArgs += DF_TM_ATOMIC_SIZE;  AM_hreg += DF_TM_SUSP_SIZE;
        args = (DF_TermPtr)(((MemPtr)args) + DF_TM_ATOMIC_SIZE);
    }
    for (; lev > 0; lev--){     //#i, for i <= lev
        DF_mkBV(newArgs, lev);
        newArgs += DF_TM_ATOMIC_SIZE;
    }
}

/* Generating a partial subsitution for the free head of a LLambda term    */
/* corresponding to an internal flex term which is known to be non-LLambda.*/
/* The partial substitution is of form:                                    */
/* (h #n ... #1)                                                           */
/* It is assumed that n is not equal to zero.                              */ 
static void HOPU_mkSubstNLL(DF_TermPtr h, int n)
{
    AM_arityError(n);
    AM_heapError(AM_hreg + DF_TM_APP_SIZE + n * DF_TM_ATOMIC_SIZE);
    DF_mkApp(AM_hreg, n, h, (DF_TermPtr)(AM_hreg + DF_TM_APP_SIZE));
       AM_hreg += DF_TM_APP_SIZE;
    for (; n > 0; n--){
        DF_mkBV(AM_hreg, n);
        AM_hreg += DF_TM_ATOMIC_SIZE;
    }
}

/* Try to solve G = ...(F a1 ... an)..., where F and G are different free  */
/* variables, and (F a1 ... an) is non-LLambda.                            */
/* Either G is bound to (F a1 ... an) or an exception is raised. In the    */
/* latter case, the caller of this function is responsible to add a        */
/* disagreement pair to the live list.                                     */
static void HOPU_bndVarNestedFlex(DF_TermPtr fhPtr, DF_TermPtr args, int nargs,
                                  int lev)
{
    HOPU_flexCheck(args, nargs, lev);
    if (DF_fvUnivCount(fhPtr) > AM_adjreg) {
        //trail(fPtr);
        DF_modVarUC(fhPtr, AM_adjreg);
    }
}

/* Try to find the (partial) structure of the substitution for a flex head  */
/* of a LLambda term corresponding to an internal flex term which is not    */
/* known to be LLambda.                                                     */
/* If the internal flex term is LLambda, HOPU_flexNestedLLambda is invoked  */
/* to generate the (parital) substitution for the outside variable, and     */
/* perform proper substitutions on the internal free variable if necessary. */
/* Otherwise, a disagreement pair is added into the live list.              */
static DF_TermPtr HOPU_flexNestedSubst(DF_TermPtr args1, int nargs1, 
                                       DF_TermPtr fhPtr, DF_TermPtr args2, 
                                       int nargs2, DF_TermPtr tmPtr, int emblev)
{
    DF_TermPtr bnd;
    int varuc = DF_fvUnivCount(fhPtr);
    if (HOPU_isLLambda(varuc, nargs2, args2)){ 
        if (fhPtr == AM_vbbreg) EM_throw(EM_HOPU_FAIL); //occurs check
        bnd = HOPU_flexNestedLLambda(args1, nargs1, varuc, tmPtr, fhPtr, args2,
                                     nargs2, emblev);
    } else {// the internal flex term is not LLambda: delay (opt possible)
        DF_TermPtr newVar;
        DF_TermPtr newTerm;
        if (fhPtr != AM_vbbreg) {
            EM_try{ 
                HOPU_bndVarNestedFlex(fhPtr, args2, nargs2, emblev);
                return tmPtr;
            } EM_catch {if (EM_CurrentExnType != EM_HOPU_FAIL) EM_reThrow();}
        }
        newVar = (DF_TermPtr)AM_hreg;
        HOPU_pushVarToHeap(AM_adjreg);
        HOPU_copyFlagGlb = TRUE;
        if ((nargs1 == 0) && (emblev == 0)) {
            bnd = newVar;
            AM_addDisPair(bnd, tmPtr);
        } else {                          
            newTerm = (DF_TermPtr)AM_hreg;
            HOPU_mkTermNLL(newVar, args1, nargs1, emblev);
            AM_addDisPair(newTerm, tmPtr);
            bnd = (DF_TermPtr)AM_hreg;
            HOPU_mkSubstNLL(newVar, emblev + nargs1);
        }
    }
    return bnd;
}

/* Try to solve G = (F a1 ... an), where F and G are different free        */
/* variables, and (F a1 ... an) is non-LLambda.                            */
/* Either G is bound to (F a1 ... an) or an exception is raised. In the    */
/* latter case, the caller of this function is responsible to add a        */
/* disagreement pair to the live list.                                     */
static void HOPU_bndVarFlex(DF_TermPtr vPtr, DF_TermPtr fPtr, DF_TermPtr fhPtr,
                            DF_TermPtr args, int nargs)
{
    AM_vbbreg = vPtr;    AM_adjreg = DF_fvUnivCount(vPtr);
    HOPU_flexCheck(args, nargs, 0);    
    if (DF_fvUnivCount(fhPtr) > AM_adjreg) {
        //trail(fPtr);
        DF_modVarUC(fhPtr, AM_adjreg);
    }
    //trail(vPtr);
    DF_mkRef((MemPtr)vPtr, fPtr);
}

/* Try to solve (F a1 ... an) = lam(k, (G b1 ... bm)), where F and G are   */
/* both free variables.                                                    */ 
/*  The arguments are:                                                     */
/*  tPtr1 : reference to the ABSTRACTION BODY of the first flex term       */
/*  h1    : reference to the flex head of the first term                   */
/*  nargs1: number of arguments of the first flex term                     */
/*  args1 : reference to the argument vector of the first flex term        */
/*  tPtr2 : reference to the ABSTRACTION BODY of the second flex term      */
/*  h2    : reference to the flex head of the second flex term             */
/*  nargs2: number of arguments of the second flex term                    */
/*  args2 : reference to the argument vector of the second flex term       */
/*  lev   : abstraction context of the second term with respect to the     */
/*          first one.                                                     */
/*                                                                         */
/*  Non-Llambda pairs could be encountered during this process, and in     */  
/*  this situation, the pair is delayed onto the disagreement list.        */
static void HOPU_flexMkSubst(DF_TermPtr tPtr1, DF_TermPtr h1, int nargs1,
                             DF_TermPtr args1, DF_TermPtr tPtr2, DF_TermPtr h2,
                             int nargs2, DF_TermPtr args2, int lev)
{
    int uc = DF_fvUnivCount(h1);
    if (HOPU_isLLambda(uc, nargs1, args1)){ //the first term is LLambda
        DF_TermPtr bndBody;
        if (h1 == h2) { //same variable (comparing addresses)
            if (HOPU_isLLambda(uc, nargs2, args2)) {
                MemPtr oldhtop = AM_hreg;
                DF_TermPtr newArgs = (DF_TermPtr)AM_hreg;
                HOPU_copyFlagGlb = FALSE;
                nargs1 = HOPU_pruneSameVar(args1, nargs1, args2, nargs2, lev);
                if ((nargs1 != nargs2) || HOPU_copyFlagGlb){
                    DF_TermPtr newVar = (DF_TermPtr)AM_hreg;
                    HOPU_pushVarToHeap(uc);
                    HOPU_mkPandRSubst(newVar, newArgs, nargs1, h1, nargs2);
                } else AM_hreg = oldhtop; //unbound
            } else {  //(F a1 ... an)[ll] = (lam(k, (F b1 ... bm)))[non-ll] 
                if (lev == 0) AM_addDisPair(tPtr1, tPtr2);  
                else {
                    MemPtr nhtop = AM_hreg + DF_TM_LAM_SIZE;
                    DF_TermPtr tmPtr = (DF_TermPtr)AM_hreg;
                    AM_heapError(AM_hreg);
                    DF_mkLam(AM_hreg, lev, tPtr2);
                    AM_hreg = nhtop;
                    AM_addDisPair(tPtr1, tmPtr);
                } //(lev != 0)
            } //tPtr2 not LLambda 
        } else {        //different variable
            int nabs;
            AM_vbbreg = h1;   AM_adjreg = uc;   //set regs for occ
            HOPU_copyFlagGlb = FALSE;       
            bndBody = HOPU_flexNestedSubst(args1, nargs1, h2, args2, nargs2, 
                                           tPtr2, lev);
            nabs = lev + nargs1;
            // trail(h1);
            if (nabs == 0) DF_mkRef((MemPtr)h1, bndBody);
            else {
                AM_embedError(nabs);
                DF_mkLam((MemPtr)h1, nabs, bndBody);
            }  
        }               //different variable
    } else {            //the first term is non-LLambda        
        if ((nargs2 == 0) && (lev == 0) && (h1 != h2)) { // (F t1 ... tm) = G  
            EM_try{ HOPU_bndVarFlex(h2, tPtr1, h1, args1, nargs1);
            } EM_catch { if (EM_CurrentExnType != EM_HOPU_FAIL) EM_reThrow(); } 
        } 
        if (lev == 0) AM_addDisPair(tPtr1, tPtr2); 
        else {
            MemPtr nhtop = AM_hreg + DF_TM_LAM_SIZE;
            DF_TermPtr tmPtr = (DF_TermPtr)AM_hreg;
            AM_heapError(AM_hreg);
            DF_mkLam(AM_hreg, lev, tPtr2);
            AM_hreg = nhtop;
            AM_addDisPair(tPtr1, tmPtr);            
        } //(lev != 0) 
    }                  //the first term is non-LLambda
}

/***************************************************************************/
/*                 BINDING FOR FLEX-RIGID                                  */
/*                                                                         */
/* Auxiliary functions for solving flex-rigid pairs.                       */
/* Non-LLambda pairs are delayed onto the disagreement list.               */
/***************************************************************************/
/* Try to find the (partial) binding of the head of a flex term correponding */
/* to a rigid atom during the process of unifying the flex term with a       */
/* rigid one. The global variable HOPU_copyFlagGlb is used to indicate       */
/* whether a new term is created during this process.                        */
/* Note it is assumed that rPtr refers to the dereference of a rigid atom    */
/* or cons.                                                                  */
static DF_TermPtr HOPU_getHead(DF_TermPtr rPtr, DF_TermPtr args, int nargs, 
                               int emblev)
{
    DF_TermPtr rtPtr;
    switch(DF_termTag(rPtr)){
    case DF_TM_TAG_CONST:{
        if (DF_constUnivCount(rPtr) > AM_adjreg){
            MemPtr newhtop;
            int ind = HOPU_constIndex(rPtr, args, nargs, emblev);
            if (ind == 0) EM_throw(EM_HOPU_FAIL);        //occurs-check
            AM_embedError(ind);
            newhtop = AM_hreg + DF_TM_ATOMIC_SIZE;
            AM_heapError(newhtop);
            HOPU_copyFlagGlb = TRUE;         //new structure is created
            rtPtr = (DF_TermPtr)AM_hreg;     //create a db on the heap top
            DF_mkBV(AM_hreg, ind);
            AM_hreg = newhtop;
        } else rtPtr = rPtr; //DF_constUnivCount(rPtr <= AM_adjreg)
        break;
    }
    case DF_TM_TAG_BVAR:   {
        int dbInd = DF_bvIndex(rPtr);
        if (dbInd > emblev){
            int ind = HOPU_bvIndex(dbInd, args, nargs, emblev);
            if (ind == 0) EM_throw(EM_HOPU_FAIL);        //occurs-check
            AM_embedError(ind);
            if (ind == dbInd) rtPtr = rPtr;  //use the old db term
            else {                           //create a db on the heap top
                MemPtr newhtop = AM_hreg + DF_TM_ATOMIC_SIZE;
                AM_heapError(newhtop);
                HOPU_copyFlagGlb = TRUE;     //new structure is created
                rtPtr = (DF_TermPtr)AM_hreg;
                DF_mkBV(AM_hreg, ind);
                AM_hreg = newhtop;   
            }
        } else rtPtr = rPtr; //dbInd <= emlev
        break;
    }
    default: { rtPtr = rPtr; break;} //other rigid head: cons,nil,int,fl,str 
    } //switch
    return rtPtr;
}

/* Create a new cons or app term on the current heap top.                 */
static void HOPU_mkConsOrApp(DF_TermPtr tmPtr, DF_TermPtr funcPtr, 
                             DF_TermPtr argvec, int nargs)
{
    MemPtr newhtop;
    if (DF_isCons(tmPtr)) {
        newhtop = AM_hreg + DF_TM_CONS_SIZE;
        AM_heapError(newhtop);
        DF_mkCons(AM_hreg, argvec);
    } else {// application
        newhtop = AM_hreg + DF_TM_APP_SIZE;
        AM_heapError(newhtop);
        DF_mkApp(AM_hreg, nargs, funcPtr, argvec);
    }
    AM_hreg = newhtop;
}

/* Try to find the (partial) binding of the head of a flex term when       */
/* unifying it with a rigid term possible under abstractions.              */
/* The arguments are:                                                      */
/*  fargs:    reference to the arguments of the flex term                  */
/*  fnargs:   number of arguments of the flex term                         */
/*  rhPtr:    reference to the rigid head                                  */
/*  rPtr:     reference to the ABSTRACTION BODY of the rigid term          */
/*  rargs:    reference to the arguments of the rigid term                 */
/*  rnargs:   number of arguments of the rigid term                        */
/*  emblev:   abstraction context of the rigid term                        */
/* The global variable HOPU_copyFlagGlb is used to indicate whether new    */
/* term is created in this process.                                        */
/* Note that if the rigid term is app or cons, it is first assumed that    */
/* a new argument vector is to be created. However, after all the args in  */
/* the binding are calculated, a checking is made on whether this is       */
/* really necessary. If it is not, the old arg vector is used, and the new */
/* one is abandoned. (Heap space for it is deallocated.)                   */ 
/* It is assumed that the flexible head and its universe count are         */
/* in registers AM_vbbreg and AM_adjreg.                                   */
static DF_TermPtr HOPU_rigNestedSubst(DF_TermPtr fargs, int fnargs, 
                                      DF_TermPtr rhPtr, DF_TermPtr rPtr,
                                      DF_TermPtr rargs, int rnargs, int emblev)
{
    rhPtr = HOPU_getHead(rhPtr, fargs, fnargs, emblev); //head of the binding
    if (rnargs == 0) return rhPtr;       //the rigid term is atomic
    else {                               //the rigid term is cons or app
        Boolean myCopyFlagHead = HOPU_copyFlagGlb, myCopyFlagArgs = FALSE;
        int i;
        MemPtr oldHreg = AM_hreg;                   //the old heap top
        MemPtr argLoc  = AM_hreg;                   //arg vector location
        DF_TermPtr newArgs = (DF_TermPtr)AM_hreg;   //new argument vector
        DF_TermPtr oldArgs = rargs;                 //old argument vector
        AM_heapError(AM_hreg + rnargs * DF_TM_ATOMIC_SIZE);   
        AM_hreg += rnargs * DF_TM_ATOMIC_SIZE;      //allocate space for argvec
        HOPU_copyFlagGlb = FALSE;
        for (i = 0; i < rnargs; i++){
            DF_TermPtr bnd;
            int        nabs;
            HN_hnorm(rargs);  nabs = AM_numAbs;     //dereference of hnf
            if (AM_rigFlag){
                bnd = HOPU_rigNestedSubst(fargs, fnargs, AM_head,
                       HOPU_lamBody(rargs), AM_argVec, AM_numArgs, nabs+emblev);
            } else { //AM_rigFlag = FALSE
                bnd = HOPU_flexNestedSubst(fargs, fnargs, AM_head, AM_argVec,
                       AM_numArgs, HOPU_lamBody(rargs), nabs+emblev);
            }
            if (nabs == 0) DF_mkRef(argLoc, bnd); //compact atomic??
            else DF_mkLam(argLoc, nabs, bnd);
            argLoc += DF_TM_ATOMIC_SIZE;  //note: abs has atomic size
            if (HOPU_copyFlagGlb) {myCopyFlagArgs=TRUE; HOPU_copyFlagGlb=FALSE;}
            rargs = (DF_TermPtr)(((MemPtr)rargs)+DF_TM_ATOMIC_SIZE); //next arg
        } //for loop
        if (myCopyFlagArgs) {
            DF_TermPtr tmPtr = (DF_TermPtr)AM_hreg; //new cons or app
            HOPU_mkConsOrApp(rPtr, rhPtr, newArgs, rnargs);
            HOPU_copyFlagGlb = TRUE;
            return tmPtr;
        } else { //myCopyFlagBody == FALSE
            AM_hreg = oldHreg; //deallocate space for the argument vector
            //note no new terms are created form any argument
            if (myCopyFlagHead){
                DF_TermPtr tmPtr = (DF_TermPtr)AM_hreg; //new cons or app
                HOPU_mkConsOrApp(rPtr, rhPtr, oldArgs, rnargs);
                HOPU_copyFlagGlb = TRUE;
                return tmPtr;
            } else  return rPtr; //myCopyFlag == FALSE, myCopyFlagBody = FALSE
        }
    }//rnargs > 0
}

/* Try to solve (F a1 ... an) = lam(k, (r b1 ... bm)), where r is rigid.   */ 
/*  The arguments are:                                                     */
/*  fPtr  : reference to the ABSTRACTION BODY of the flex term             */
/*  fhPtr : reference to the flex head                                     */
/*  fnargs: number of arguments of the flex term                           */
/*  fargs : reference to the argument vector of the flex term              */
/*  rPtr  : reference to the ABSTRACTION BODY of the rigid term            */
/*  rhPtr : reference to the rigid head (Note it could be cons)            */
/*  rnargs: number of arguments of the rigid term                          */
/*  rargs : reference to the argument vector of the rigid term             */
/*                                                                         */
/*  Non-Llambda pairs could be encountered during this process, and in     */  
/*  this situation, the pair is delayed onto the disagreement list.        */
static void HOPU_rigMkSubst(DF_TermPtr fPtr, DF_TermPtr fhPtr, int fnargs,
                            DF_TermPtr fargs, DF_TermPtr rPtr, DF_TermPtr rhPtr,
                            int rnargs, DF_TermPtr rargs, int emblev)
{
    int uc = DF_fvUnivCount(fhPtr);
    if (HOPU_isLLambda(uc, fnargs, fargs)){//Llambda pattern
        DF_TermPtr bndBody;                //abs body of bnd of the fv
        int nabs;
        AM_vbbreg = fhPtr; AM_adjreg = uc; //set regs for occurs check
        bndBody   = HOPU_rigNestedSubst(fargs, fnargs, rhPtr, rPtr,
                                        rargs, rnargs, emblev);
        nabs      = emblev + fnargs; //# abs in the front of the binding
        //trail (fhPtr);    
        if (nabs == 0) DF_mkRef((MemPtr)fhPtr, bndBody);
        else {
            AM_embedError(nabs);
            DF_mkLam((MemPtr)fhPtr, nabs, bndBody);
        }
    } else {                              //non-Llambda pattern
        if (emblev == 0) AM_addDisPair(fPtr, rPtr);
        else {
            MemPtr nhtop = AM_hreg + DF_TM_LAM_SIZE;
            DF_TermPtr tmPtr = (DF_TermPtr)AM_hreg;
            AM_heapError(AM_hreg);
            DF_mkLam(AM_hreg, emblev, rPtr);
            AM_hreg = nhtop;
            AM_addDisPair(fPtr, tmPtr);
        } // (emblev != 0)
    }                                     //non-LLambda pattern
}

/***************************************************************************/
/*             TERM SIMPLIFICATION (RIGID-RIGID)                           */
/*                                                                         */
/* Auxiliary functions for solving rigid-rigid pairs.                      */
/***************************************************************************/

/* Matching heads of two rigid terms. Eta-expansion is considered when     */
/* necessary. It is assumed that the heads have been dereferenced.         */
static void HOPU_matchHeads(DF_TermPtr hPtr1, DF_TermPtr hPtr2, int nabs)
{
    switch(DF_termTag(hPtr1)){
    case DF_TM_TAG_CONST:{
        if (!(DF_isConst(hPtr2) && (DF_sameConsts(hPtr1, hPtr2)))) 
            EM_throw(EM_HOPU_FAIL);
        if (DF_isTConst(hPtr1)){ //(first-order) unify type environments
            EM_try {
                HOPU_typesUnify(DF_constType(hPtr1), DF_constType(hPtr2), 
                                AM_cstTyEnvSize(DF_constTabIndex(hPtr1)));
            } EM_catch {
                if (EM_CurrentExnType == EM_TY_UNI_FAIL) EM_throw(EM_HOPU_FAIL);
                else EM_reThrow();
            }            
        }
        break;
    }
    case DF_TM_TAG_BVAR: {
        if (!DF_isBV(hPtr2)) EM_throw(EM_HOPU_FAIL);
        else {                   
            int ind = DF_bvIndex(hPtr2) + nabs; //lifting for eta-expansion
            AM_embedError(ind);
            if (DF_bvIndex(hPtr1) != ind) EM_throw(EM_HOPU_FAIL);
        }
        break;
    }
    case DF_TM_TAG_NIL:  { if (!DF_isNil(hPtr2)) EM_throw(EM_HOPU_FAIL); break;}
    case DF_TM_TAG_INT:  {
        if (!(DF_isInt(hPtr2) && (DF_intValue(hPtr2) == DF_intValue(hPtr1))))
            EM_throw(EM_HOPU_FAIL);
        break;
    }
    case DF_TM_TAG_FLOAT:{
        if (!(DF_isFloat(hPtr2)&&(DF_floatValue(hPtr2)==DF_floatValue(hPtr1))))
            EM_throw(EM_HOPU_FAIL);
        break;
    }
    case DF_TM_TAG_STR:  {
        if (!(DF_isStr(hPtr2) && (DF_sameStrs(hPtr1, hPtr2))))
            EM_throw(EM_HOPU_FAIL);
        break;
    }
    case DF_TM_TAG_CONS: { 
        if (!(DF_isCons(hPtr2))) EM_throw(EM_HOPU_FAIL);
        break;
    }
    } //switch
}


/* Set up PDL by sub problems resulted from rigid-rigid pairs upon         */
/* successful matching of their heads. Eta-expansion is performed on-a-fly */
/* when necessary.                                                         */
void HOPU_setPDL(MemPtr args1, MemPtr args2, int nargs, int nabs)
{
    if (nabs == 0){ //no need for eta-expansion
        AM_pdlError(nargs * 2);
        for (; nargs > 0; nargs --){
            AM_pushPDL(args1);   args1 +=  DF_TM_ATOMIC_SIZE; 
            AM_pushPDL(args2);   args2 +=  DF_TM_ATOMIC_SIZE;
        }
    } else {        //nabs > 0 (eta-expansion)
        AM_pdlError((nargs + nabs) * 2);
        AM_heapError(AM_hreg + nargs*DF_TM_SUSP_SIZE + nabs*DF_TM_ATOMIC_SIZE);
        for (; nargs > 0; nargs --){ //[|ai, 0, nabs, nil|]
            AM_pushPDL(args1);   AM_pushPDL(AM_hreg);
            DF_mkSusp(AM_hreg, 0, nabs, DF_termDeref((DF_TermPtr)args2), 
                      DF_EMPTY_ENV);
            AM_hreg += DF_TM_SUSP_SIZE;
            args1 += DF_TM_ATOMIC_SIZE; args2 += DF_TM_ATOMIC_SIZE;
        }
        for (; nabs > 0; nabs --){   // bv(i)
            AM_pushPDL(args1);   AM_pushPDL(AM_hreg);
            DF_mkBV(AM_hreg, nabs);
            args1 += DF_TM_ATOMIC_SIZE; AM_hreg += DF_TM_ATOMIC_SIZE;
        }
    }
}

/***************************************************************************/
/*            HIGHER_ORDER PATTERN UNIFICATION                             */
/*                                                                         */   
/*  The main routines of this file.                                        */
/***************************************************************************/
/* Perform higher-order pattern unification over the pairs delayed on the  */
/* PDL stack. The PDL stack is empty upon successful termination of this   */
/* procedure.                                                              */
void HOPU_patternUnify()
{
    DF_TermPtr tPtr1, tPtr2,  //pointers to terms to be unified
               hPtr,          //pointer to head of hnf 
               args;          //arg vec of hnf
    Flag       rig,   cons;   //rigid flag and cons flags
    int        nabs,  nargs;  //binder length and # of arguments of hnf
    while (AM_nemptyPDL()){
        //retrieve the pair of terms on the current top of PDL
        tPtr1 = (DF_TermPtr)AM_popPDL(); tPtr2 = (DF_TermPtr)AM_popPDL();
        HN_hnorm(tPtr1); //hnorm tPtr1 
        hPtr  = AM_head; args = AM_argVec; nabs = AM_numAbs; nargs = AM_numArgs;
        rig   = AM_rigFlag;      //bookkeeping relevant info of hnf of tPtr1
        HN_hnorm(tPtr2); //hnorm tPtr2
        if (rig){
            if (AM_rigFlag){// rigid - rigid
                if (nabs > AM_numAbs) {
                    nabs = nabs - AM_numAbs;   //reuse nabs
                    HOPU_matchHeads(hPtr, AM_head, nabs);
                    HOPU_setPDL((MemPtr)args,(MemPtr)AM_argVec,AM_numArgs,nabs);
                } else {      //nabs <= AM_numAbs
                    nabs = AM_numAbs - nabs;   //reuse nabs
                    HOPU_matchHeads(AM_head, hPtr, nabs);
                    HOPU_setPDL((MemPtr)AM_argVec, (MemPtr)args, nargs, nabs);
                } 
            } else {        // rigid - flex
                DF_TermPtr rigBody  = HOPU_lamBody(tPtr1);
                DF_TermPtr flexBody = HOPU_lamBody(tPtr2);
                if (nabs < AM_numAbs) { //eta expand rigid term first
                    nabs = AM_numAbs - nabs;   //reuse nabs
                    rigBody = HOPU_etaExpand(&hPtr, &args, nargs, nabs);
                    HOPU_rigMkSubst(flexBody, AM_head, AM_numArgs, AM_argVec, 
                                    rigBody, hPtr, (nargs+nabs), args, 0);
                } else HOPU_rigMkSubst(flexBody,AM_head, AM_numArgs, AM_argVec, 
                                       rigBody,hPtr,nargs,args,nabs-AM_numAbs);
            }               // rigid-flex
        } else { //(rig == FALSE)
            DF_TermPtr absBody1 = HOPU_lamBody(tPtr1);
            DF_TermPtr absBody2 = HOPU_lamBody(tPtr2);
            if (AM_rigFlag){// flex - rigid
                if (AM_numAbs < nabs) { //eta expand rigid term first
                    nabs = nabs - AM_numAbs;   //reuse nabs
                    absBody2 = HOPU_etaExpand(&AM_head, &AM_argVec, AM_numArgs,
                                              AM_numAbs);
                    HOPU_rigMkSubst(absBody1, hPtr, nargs, args, absBody2, 
                                    AM_head, AM_numArgs+nabs, AM_argVec, 0);
                }else HOPU_rigMkSubst(absBody1,hPtr,nargs,args,absBody2,AM_head,
                                      AM_numArgs,AM_argVec,AM_numAbs-nabs);
            } else {        // flex - flex
                if (AM_numAbs > nabs)
                    HOPU_flexMkSubst(absBody1, hPtr, nargs, args, absBody2, 
                                     AM_head, AM_numArgs, AM_argVec, 
                                     AM_numAbs-nabs);
                else HOPU_flexMkSubst(absBody2, AM_head, AM_numArgs, AM_argVec,
                                      absBody1,hPtr,nargs,args,nabs-AM_numAbs);
            }               // flex - flex
        }        //(rig == FALSE)
    } // while (AM_nemptyPDL())
}

#endif //HOPU_C
