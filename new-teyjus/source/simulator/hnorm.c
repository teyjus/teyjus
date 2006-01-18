/****************************************************************************/
/*                                                                          */
/* File hnorm.c.                                                            */
/* This file contains the head normalization routines.                      */
/* These procedures are based on the suspension calculus, and the reduction */
/* strategy with lazy reduction, lazy substitution and lazy heap            */
/* commitment is chosen. A SML realization of this strategy is described in */
/* paper "Choices in Representation and Reduction Strategies for Lambda     */
/* Terms in Intersional Contexts".                                          */
/****************************************************************************/

#ifndef HNORM_C
#define HNORM_C

#include <stdlib.h>
#include "dataformat.h"
#include "abstmachine.h"
#include "mctypes.h"
#include "hnorm.h"

//to be removed: for debug
#include <stdio.h>
#include "print.h"

/************************************************/
/* initialize relevant registers                */
/************************************************/
static void HN_initRegs()
{
    AM_numabs  = AM_numargs = 0;
    AM_head    = AM_argvec  = NULL;
}

/************************************************/
/* set relevant registers in termination cases. */
/************************************************/

/* for cons: note that AM_numargs is not set (which must remain 0) */
static void HN_setRegsCons() 
{
    AM_consflag = AM_rigflag = ON;
}

/* for rigid atoms */
static void HN_setRegsRig(DF_TERM_PTR head)  
{
    AM_consflag = OFF;
    AM_rigflag = ON;
    AM_head = head;
}

/* for free variable */
static void HN_setRegsFlex(DF_TERM_PTR head)
{
    AM_consflag = AM_rigflag = OFF;
    AM_head = head;
}

/*****************************************************************************/
/* a global(to file hnorm.c) encoding  of the explicit suspension environment*/
/* and simple checking and updating functions on this environment            */
/*****************************************************************************/

/* environment of the implicit suspension, which is initialized to empty*/
static DF_EMBEDLEV ol, nl;
static DF_ENV_PTR  envlist;

/* clean the environment to empty */
static void HN_setEmptyEnv()
{
    ol = 0;
    nl = 0;
    envlist = DF_EMPTYENV;
}

/* set the environment according to given values */
static void HN_setEnv(DF_EMBEDLEV o, DF_EMBEDLEV n, DF_ENV_PTR e)
{
    ol = o;
    nl = n;
    envlist = e;
}

/* is an empty environment? */
static BOOLEAN HN_isEmptyEnv()
{
    return ((ol == 0) && (nl == 0));
}


/***************************************************************************/
/* Some functions relevant to term creation and updating commonly          */
/* used in hn routines                                                     */
/***************************************************************************/

/* update the content of <tp> to a reference term referring <target>, after 
   trailing the content of <tp> */  
static void HN_updateToRef(DF_TERM_PTR tp, DF_TERM_PTR target)
{
    //to add trail *tp;
    DF_MkRef_(tp, target);
}

/* create a bound variable on the current heap top guarded by heap
   overflow detection*/
static void HN_newBV(DF_EMBEDLEV dbind)
{
    MEM_PTR newhtop = DF_IncAtomic(AM_hreg);
    AM_heapError(newhtop);

    DF_MkBV_((DF_TERM_PTR)AM_hreg,dbind);
    AM_hreg = newhtop;
}

/* create a suspension on the current heap top guarded by heap overflow
   detection */
static void HN_newSusp(DF_TERM_PTR skp, DF_EMBEDLEV sol, DF_EMBEDLEV snl,
                       DF_ENV_PTR  senv)
{
    MEM_PTR newhtop = DF_IncSusp(AM_hreg);
    AM_heapError(newhtop);

    DF_MkSusp_((DF_TERM_PTR)AM_hreg, sol, snl, skp, senv);
    AM_hreg = newhtop;
}

/* create a suspension on the given location guarded by heap overflow 
   detection */
static void HN_newSuspOnLoc(DF_TERM_PTR skp, DF_EMBEDLEV sol, DF_EMBEDLEV snl,
                            DF_ENV_PTR  senv, DF_TERM_PTR *loc)
{
    MEM_PTR newloc = DF_IncSusp((MEM_PTR)*loc);
    AM_heapError(newloc);
    
    DF_MkSusp_(*loc, sol, snl, skp, senv);
    *loc = (DF_TERM_PTR)newloc;
}

/* create a lam on the current heap top guarded by heap overflow detection */
static void HN_newLam(DF_TERM_PTR body, DF_EMBEDLEV embedlevel)
{
    MEM_PTR newhtop = DF_IncLam(AM_hreg);
    AM_heapError(newhtop);
    
    DF_MkLam_((DF_TERM_PTR)AM_hreg, embedlevel, body);
    AM_hreg = newhtop;
}

/* create a app from AM_argvec and AM_head on the current heap top guarded 
   by heap overflow detection. 
*/
static void HN_newApp()
{
    DF_TERM fun;
    MEM_PTR newhtop = DF_IncApp(AM_hreg);
    AM_heapError(newhtop);
    
    DF_MkRef_(&fun, AM_head); //note a reference is made as app's functor
    DF_MkApp_((DF_TERM_PTR)AM_hreg, AM_numargs, &fun, AM_argvec);
    AM_hreg = newhtop;
}
 
/* create a cons from AM_argvec on the current heap top guarded by heap 
   overflow detection.*/
static void HN_newCons()
{
    MEM_PTR newhtop = DF_IncCons(AM_hreg);
    AM_heapError(newhtop);
    DF_MkCons_((DF_TERM_PTR)AM_hreg, AM_argvec);
    AM_hreg = newhtop;
}


/***************************************************************************/
/* functions for eagerly evaluating implicit suspensions over atomic terms */
/* needed when an implicit suspension has to be made explicitly on heap    */
/***************************************************************************/

/* [|skp, 0, adj, nil|]: solve implicit renumber suspension when it 
   is to be added into the environment. When skp is atomic, the suspension
   is eagerly solved; otherwise it is committed on the current heap top.
   Combination is performed if skp is itself a suspension. */
static DF_TERM_PTR HN_renumberAsEnv(DF_TERM_PTR skp, DF_EMBEDLEV adj)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    switch (DF_Tag(skp)){    
    case DF_TM_TAG_VAR:
    case DF_TM_TAG_CONST:
    case DF_TM_TAG_INT:
    case DF_TM_TAG_FLOAT:
    case DF_TM_TAG_NIL:
    case DF_TM_TAG_STR:
    case DF_TM_TAG_STREAM:
    { 
        rttp = skp;
        break;
    }
    case DF_TM_TAG_LAM:
    case DF_TM_TAG_CONS:
    case DF_TM_TAG_APP:
    { 
        rttp = (DF_TERM_PTR)AM_hreg;
        HN_newSusp(skp, 0, adj, DF_EMPTYENV);
        break;
    }
    case DF_TM_TAG_SUSP:
    { //[|[|myskp,myol,mynl,myenv|],0,adj,nil|] -> [|myskp,myol,mynl+adj,myenv|]
        DF_TERM_PTR    myskp   = DF_Deref(DF_SuspTermSkel(skp));
        DF_EMBEDLEV    myol    = DF_SuspOL(skp),   mynl = DF_SuspNL(skp);
        DF_ENV_PTR     myenv   = DF_SuspEnv(skp);
        DF_PREEMBEDLEV newnl   = mynl+adj;
        
        AM_embedError(newnl);

        rttp = (DF_TERM_PTR)AM_hreg;
        HN_newSusp(skp, myol, newnl, myenv);
        break;
    }                                
    case DF_TM_TAG_BVAR:                   //[|#i, 0, adj, nil |] -> #(i-0+adj)
    {    
        DF_PREEMBEDLEV newind = DF_BVIndex(skp)+adj;

        AM_embedError(newind);

        rttp = (DF_TERM_PTR)AM_hreg;
        HN_newBV(newind);
        break;
    }
    case DF_TM_TAG_REF:     // to be removed: for debug
    {
        rttp = skp;
        printf("error: reference in HN_suspIntoEnv\n");
        break;
    }
    }
    return rttp;
}


/* [|skp, 0, adj, nil|]: solve implicit renumber suspension when it 
   is to become a term argument. 
   The differences from HN_renumberAsEnv are:
   1) if the result of the suspension is an atom and not a fv or const,
      this atom will be created into the position referred by <loc>;
      otherwise, a reference referring the result will be created into <loc>.
   2) if new suspension has to be built, it is built in the location
      starting from <*sploc>.
   3) this function is in charge of checking whether <adj> is 0.
*/
   
static void HN_renumberAsArg(DF_TERM_PTR skp, DF_EMBEDLEV adj, 
                             DF_TERM_PTR loc, DF_TERM_PTR *sploc)
{
    switch (DF_Tag(skp)){    
    case DF_TM_TAG_VAR:
    case DF_TM_TAG_CONST:
    {
        DF_MkRef_(loc, skp);
        break;
    }
    case DF_TM_TAG_INT:
    case DF_TM_TAG_FLOAT:
    case DF_TM_TAG_NIL:
    case DF_TM_TAG_STR:
    case DF_TM_TAG_STREAM:
    { 
        DF_CopyAtom_(skp, loc);
        break;
    }
    case DF_TM_TAG_LAM:
    case DF_TM_TAG_CONS:
    case DF_TM_TAG_APP:
    { 
        if (adj == 0) DF_MkRef_(loc, skp);
        else {
            DF_MkRef_(loc, *sploc); //make reference to new susp
            HN_newSuspOnLoc(skp, 0, adj, DF_EMPTYENV, sploc); //create susp
        }
        break;
    }
    case DF_TM_TAG_SUSP:
    { 
        if (adj == 0) DF_MkRef_(loc, skp);
        else {
            DF_TERM_PTR    myskp = DF_Deref(DF_SuspTermSkel(skp));
            DF_EMBEDLEV    myol  = DF_SuspOL(skp),  mynl = DF_SuspNL(skp);
            DF_ENV_PTR     myenv = DF_SuspEnv(skp);
            DF_PREEMBEDLEV newnl = mynl+adj;
        
            AM_embedError(newnl);

            DF_MkRef_(loc, *sploc); //make reference to new susp
            HN_newSuspOnLoc(myskp, myol, newnl, myenv, sploc); //create susp
            break;
        }          
    }
    case DF_TM_TAG_BVAR:                   //[|#i, 0, adj, nil |] -> #(i-0+adj)
    {    
        DF_PREEMBEDLEV newind = DF_BVIndex(skp)+adj;
        AM_embedError(newind);

        DF_MkBV_(loc, newind);
        break;
    }
    case DF_TM_TAG_REF:     // to be removed: for debug
    {
        printf("error: reference in HN_suspIntoEnv\n");
        break;
    }
    }
}


/* [|#ind, myol, mynl, myenv|]: solve implicit suspension over bound variable
   when it is to be added into environment. 
   The suspension is eagerly evaluated till a non-suspension term 
   or a non-trivial suspension is resulted.
   In the latter case, a suspension is explicitly committed on the heap top.
*/
static DF_TERM_PTR HN_BVSuspAsEnv(DF_EMBEDLEV ind, DF_EMBEDLEV myol, 
                                  DF_EMBEDLEV mynl, DF_ENV_PTR myenv)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    if (ind > myol){          //[|#i,ol,nl,envlist|]->#(i-ol+nl), where i>ol
        DF_PREEMBEDLEV newind = ind - myol + mynl;
        AM_embedError(newind);

        rttp = (DF_TERM_PTR)AM_hreg;
        HN_newBV(newind);
    } else {
        DF_ENV_PTR envitem = DF_EnvNth(myenv, ind); //ith in env 
        DF_EMBEDLEV nladj = mynl - DF_EnvIndex(envitem);
        
        if (DF_IsDummy(envitem)){//[|#i,ol,nl,..@l..|]->#(nl-l), where i<=ol
            //to be removed: for debug
            if (nladj==0) printf("error: [|#i,ol,nl,..@l..|]->#0\n");
        
            rttp = (DF_TERM_PTR)AM_hreg;
            HN_newBV(nladj); 
        } else { //DF_IsPairEnv(envitem)
            DF_TERM_PTR tp = DF_Deref(DF_EnvTerm(envitem)); //term in pair env

            if (nladj==0) rttp = tp;
            else rttp = HN_renumberAsEnv(tp, nladj);
        }
    } // ind <= ol
    return rttp;
}    


/* [|#i, myol, mynl, myenv |]: solve implicit suspension on bound variable 
   when it is to become a term argument. 
   The differences from HN_BVSuspAsEnv:
   1) the environment of implicit suspension is given by parameters.
   2) if the result is a non-suspention, either the term or a reference  
      is created on the location given by <loc>; Otherwise, a suspension is
      created on <*sploc>, and a reference to it is created on <loc>.
*/
static void HN_BVSuspAsArg(DF_TERM_PTR bv, DF_EMBEDLEV myol, DF_EMBEDLEV mynl, 
                           DF_ENV_PTR myenv, DF_TERM_PTR loc, 
                           DF_TERM_PTR *sploc)
{
    DF_EMBEDLEV ind = DF_BVIndex(bv); //index of the bv
    if (ind > myol){           //[|#i,ol,nl,envlist|]->#(i-ol+nl), where i>ol
        DF_PREEMBEDLEV newind = ind - myol + mynl;
        AM_embedError(newind);

        DF_MkBV_(loc, newind);
    } else {
        DF_ENV_PTR envitem = DF_EnvNth(myenv, ind); //ith item in env
        DF_EMBEDLEV nladj = mynl - DF_EnvIndex(envitem);// can't have embed err
        
        if (DF_IsDummy(envitem)){//[|#i,ol,nl,..@l..|]->#(nl-l), where i<=ol
            //to be removed: for debug
            if (nladj==0) printf("error: [|#i,ol,nl,..@l..|]->#0\n");
            
            DF_MkBV_(loc, nladj);
        } else { //DF_IsPairEnv(envitem)
            DF_TERM_PTR tp = DF_Deref(DF_EnvTerm(envitem)); //term in pair env
            HN_renumberAsArg(tp, nladj, loc, sploc);
        }
    }
}


/* [|skp, myol, mynl, myenv|]: solve implicit suspension when it is to be added
   into environment. The implicit suspension is eagerly evaluated till a 
   non-suspension term or a non-trivial suspension is resulted. In the 
   latter case, the suspension is explicitly committed on the current heap
   top.
*/ 
       
static DF_TERM_PTR HN_suspAsEnv(DF_TERM_PTR skp, DF_EMBEDLEV myol, 
                                DF_EMBEDLEV mynl, DF_ENV_PTR myenv)
{
    DF_TERM_PTR rttp;  // term pointer to be returned
    switch(DF_Tag(skp)){   //[|c, ol, nl, envlist|] -> c
    case DF_TM_TAG_VAR:
    case DF_TM_TAG_CONST:
    case DF_TM_TAG_INT:
    case DF_TM_TAG_FLOAT:
    case DF_TM_TAG_NIL:
    case DF_TM_TAG_STR:
    case DF_TM_TAG_STREAM:
    {        
        rttp = skp;
        break;
    }
    case DF_TM_TAG_LAM:
    case DF_TM_TAG_CONS:
    case DF_TM_TAG_SUSP:
    case DF_TM_TAG_APP:
    {
        rttp = (DF_TERM_PTR)AM_hreg;
        HN_newSusp(skp, myol, mynl, myenv);
        break;
    }
    case DF_TM_TAG_BVAR:
    {
        DF_EMBEDLEV  dbind = DF_BVIndex(skp);
        rttp = HN_BVSuspAsEnv(dbind, myol, mynl, myenv);
        break;
    }
    // to be removed: for debug
    case DF_TM_TAG_REF:
    {
        printf("error: reference in HN_suspAsEnv\n");
        rttp = skp;
        break;
    }
    }
    return rttp;
}


/* [|skp, ol, nl, envlist|]: solve implicit suspension when it is to 
   become term argument.
   The differences from HN_suspAsEnv:
   1) the environment of implicit suspension is given by parameters.
   2) if the result is a non-suspention, either the term or a reference  
      is created on the location given by <loc>; Otherwise, a suspension is
      created on <*sploc>, and a reference to it is created on <loc>.
   3) a flag <*changed> is set denoting whether (result of) the suspension
      is different from <skp>.
*/    
static void HN_suspAsArg(DF_TERM_PTR skp, DF_EMBEDLEV myol, DF_EMBEDLEV mynl,
                         DF_ENV_PTR myenv, DF_TERM_PTR loc, 
                         DF_TERM_PTR *sploc, BOOLEAN *changed)
{
    switch(DF_Tag(skp)){   //[|c, ol, nl, envlist|] -> c
    case DF_TM_TAG_VAR:
    case DF_TM_TAG_CONST:
    {
        DF_MkRef_(loc, skp);
        break;
    }
    case DF_TM_TAG_INT:
    case DF_TM_TAG_FLOAT:
    case DF_TM_TAG_NIL:
    case DF_TM_TAG_STR:
    case DF_TM_TAG_STREAM:
    {        
        DF_CopyAtom_(skp, loc);
        break;
    }
    case DF_TM_TAG_LAM:
    case DF_TM_TAG_CONS:
    case DF_TM_TAG_SUSP:
    case DF_TM_TAG_APP:
    {
        DF_MkRef_(loc, *sploc);
        HN_newSuspOnLoc(skp, myol, mynl, myenv, sploc);
        *changed = TRUE;
        break;
    }
    case DF_TM_TAG_BVAR:
    {
        HN_BVSuspAsArg(skp, myol, mynl, myenv, loc, sploc);
        *changed = TRUE;
        break;
    }
    // to be removed: for debug
    case DF_TM_TAG_REF:
    {
        printf("error: reference in HN_suspAsArg\n");
        break;
    }
    }
}

/*****************************************************************************/
/* functions for creating (modifiying) environment list of the implicit susp */
/*****************************************************************************/

/* add n (n > 0) dummy environment items to the front of the current 
   environment list:
   @<nl+n-1>::...::@<nl>::<envlist>
   New dummy env items are created on the current heap top guarded by heap
   overflow detection.
   <nl> and <envlist> are given by the global environment of this file.
*/
static DF_ENV_PTR HN_addNDummyEnv(int n)
{
    int i;
    DF_ENV_PTR lastenv = envlist, currenv;

    AM_heapError(DF_IncNEnvDummy(AM_hreg, n));

    for (i = 0; i < n; i++){
        currenv = (DF_ENV_PTR)AM_hreg;
        AM_hreg = (MEM_PTR)DF_MkDummyEnv((DF_ENV_PTR)AM_hreg, lastenv, nl+i);
        lastenv = currenv;
    }
    return currenv;
}

/* add n (n > 0) pair environment items to the front of the current 
   environment list:
   (tn,<nl>)::...::(t1,<nl>)::<envlist>,
   t1, ..., tn are calculated from the current implicit suspension and the 
   n terms a1,...,an referred by <argvec> as the following:
   1) if the imp susp has an empty env, then ti=ai;
   2) else ai is the result of eagerly evaluating the susp 
      [|ti,<myol>,<mynl>,<myenv>|] over atoms.  
 */
 
static DF_ENV_PTR HN_addNPair(DF_TERM_PTR argvec, DF_EMBEDLEV myol, 
                              DF_EMBEDLEV mynl, DF_ENV_PTR myenv, int n)
{
    int i;
    DF_ENV_PTR   lastenv = envlist, currenv;
    DF_TERM_PTR  arg; //for each argument in argvec
    
    DF_ENV_PTR   myenvlist = (DF_ENV_PTR)AM_hreg;
    MEM_PTR      newhtop = DF_IncNEnvPair(AM_hreg,n);

    AM_heapError(newhtop);
    AM_hreg = newhtop; //spare space for n pair env items

    for (i = 1; i<= n; i++) {
        arg = DF_Deref(argvec);
        currenv = myenvlist;
        myenvlist = DF_MkEnv(myenvlist, lastenv, nl, 
                             HN_suspAsEnv(arg, myol, mynl, myenv));
        lastenv = currenv;
        argvec = (DF_TERM_PTR)DF_IncAtomic((MEM_PTR)argvec);
    }
    return currenv;
}   



/* add n (n > 0) pair environment items to the front of the current 
   environment list:
   (tn,<nl>)::...::(t1,<nl>)::<envlist>, where t1...tn are referred by
   <argvec>
 */
static DF_ENV_PTR HN_addNPairEmpEnv(DF_TERM_PTR argvec, int n)
{
    int i;
    DF_ENV_PTR   lastenv = envlist, currenv;
    DF_TERM_PTR  arg; //for each argument in argvec

    MEM_PTR      newhtop = DF_IncNEnvPair(AM_hreg,n);
    AM_heapError(newhtop);

    for (i = 1; i <= n; i++) {
        arg = DF_Deref(argvec);
        currenv = (DF_ENV_PTR)AM_hreg;
        AM_hreg = (MEM_PTR)DF_MkEnv((DF_ENV_PTR)AM_hreg, lastenv, 0, arg);
        lastenv = currenv;   
        argvec = (DF_TERM_PTR)DF_IncAtomic((MEM_PTR)argvec);
    }
    return currenv;
}   


/****************************************************************************/
/* functions for creating application argument vector                       */
/****************************************************************************/

/* copy an argument vector beginning from <argvec> with length <arity>
   onto the top of heap. (used in unfolding nested applications)
   Note that a reference is made if a argument in <argvec> is free variable
   for destructive changes. (Note that only atomic sized terms can be
   directly put into argument vectors)
*/
static void HN_copyArgs(DF_TERM_PTR argvec, DF_ARITY arity)
{
    int i;
    for (i = 1; i <= arity; i++){
        if (DF_IsFV(argvec)) 
            AM_hreg = (MEM_PTR)DF_MkRef((DF_TERM_PTR)AM_hreg, argvec);
        else AM_hreg = (MEM_PTR)DF_CopyAtom(argvec, (DF_TERM_PTR)AM_hreg);

        argvec = (DF_TERM_PTR)DF_IncAtomic((MEM_PTR)argvec);
    }
}


/* create a argvec from <argvec> such that implicit suspensions are pushed
   over each argument on the current heap top. 
   Such susps over atomic terms are eagerly evaluated.
   Implicit suspensions are given by parameters. <*sploc> is the location
   where suspension is to be created; <*changed> is a flag denoting whether
   the new argvec is different from <argvec>.
*/
static void HN_pushSuspOverArgs(DF_TERM_PTR argvec, DF_ARITY arity,
            DF_EMBEDLEV myol, DF_EMBEDLEV mynl, DF_ENV_PTR  myenv,
            DF_TERM_PTR *sploc, BOOLEAN *changed)
{
    int i;
    DF_TERM_PTR arg; //one argument in argvec
    DF_TERM_PTR myargvec = (DF_TERM_PTR)AM_hreg;

    for (i = 1; i <= arity; i++){
        arg = DF_Deref(argvec);
        HN_suspAsArg(arg, myol, mynl, myenv, myargvec, sploc, changed);
        myargvec = (DF_TERM_PTR)DF_IncAtomic((MEM_PTR)myargvec);
        argvec   = (DF_TERM_PTR)DF_IncAtomic((MEM_PTR)argvec);
    }
}

/* arrange an argument vector for application and set AM_numargs and AM_argvec
   registers.
   If there is no nested app, the old argvec (given by <argvec>) is reused;
   Otherwise the argvecs of nested apps are unfolded onto the current heap 
   top. 
   The flag <changed> is used to denote whether the new argvec is made or 
   the old one is reused. 
*/
static BOOLEAN HN_makeArgvecEmpEnv(DF_TERM_PTR argvec, DF_ARITY arity)
{
    BOOLEAN changed; //flag denoting if new argvec is made or the old is reused
    if (AM_numargs == 0) {//no nested app
        AM_argvec = argvec; //reuse the old argvec
        AM_numargs = arity;
        changed = FALSE;
    } else {//unfold nested app
        DF_TERM_PTR newargvec = (DF_TERM_PTR)AM_hreg; 
        DF_PREARITY newarity  = arity + AM_numargs;
        MEM_PTR     newhtop   = DF_IncNAtomic(AM_hreg, newarity);
        
        AM_arityError(newarity);
        AM_heapError(newhtop);

        HN_copyArgs(AM_argvec, AM_numargs);//lay out inner argvec
        HN_copyArgs(argvec, arity); //lay out top-level argvec
        
        AM_argvec = newargvec;
        AM_numargs = newarity;
        changed = TRUE;
    }
    return changed;
}


/* arrange an argument vector for application and set AM_numargs and AM_argvec
   registers, when implicit suspensions have to be pushed over the arguments.
   Such suspensions are eagerly evaluated on atomic terms.
   If there is no nested apps and there is no changes are made on argument for
   pushing over susps, the old argvec (given by <argvec>) is reused;
   Otherwise, new argvec is made on the current heap top.
*/  
static BOOLEAN HN_makeArgvec(DF_TERM_PTR argvec, DF_ARITY arity, 
                             DF_EMBEDLEV myol, DF_EMBEDLEV mynl, 
                             DF_ENV_PTR myenv)
{
    BOOLEAN changed; //flag denoting if new argvec is made or the old is reused
    DF_TERM_PTR sploc; //place where susps are to be created
    DF_TERM_PTR newargvec = (DF_TERM_PTR)AM_hreg; //place for new argvec

    //unfold nested app first when necessary
    if (AM_numargs == 0){//no nested app
        //spare space for argvec
        sploc = (DF_TERM_PTR)DF_IncNAtomic(AM_hreg, arity);
        AM_heapError((MEM_PTR)sploc);

        AM_numargs = arity;
        changed = FALSE; //no change is made for unfolding nested app
    } else { // unfold nested app
        DF_PREARITY newarity = arity + AM_numargs;
 
        AM_arityError(newarity);
        //spare space for argvec
        sploc = (DF_TERM_PTR)DF_IncNAtomic(AM_hreg, newarity);
        AM_heapError((MEM_PTR)sploc);
        
        HN_copyArgs(AM_argvec, AM_numargs); //lay out inner argvec

        AM_numargs = newarity;
        changed = TRUE; //changes has already be made 
    }
    
    //push susp over top-level args
    HN_pushSuspOverArgs(argvec, arity, myol, mynl, myenv, &sploc, &changed);
    
    if (changed) {//changes because of unfold or build susp
        AM_hreg = (MEM_PTR)sploc;
        AM_argvec = newargvec;
    } else AM_argvec = argvec; //no change, reuse the old arg vector

    return changed;
}


/****************************************************************************/
/* function for pushing suspension over n abstractions and committing the   */
/* result on the current heap top.                                          */
/****************************************************************************/

/* the implicit susp is given by the global env variables of this file      
   the imp susp is given from the global susp env variables of this file */
static DF_TERM_PTR HN_pushSuspOverLam(DF_TERM_PTR lamSkp)
{
    DF_TERM_PTR    rttp; //term pointer to be returned 
    DF_TERM_PTR    susp; //explicit susp as the lam body in the result
    DF_EMBEDLEV    numabs =DF_LamEmbedLev(lamSkp);
    DF_PREEMBEDLEV newol = ol + numabs, newnl = nl + numabs;
    MEM_PTR        newhtop = DF_IncSuspLamNDummyEnv(AM_hreg, numabs);
    DF_ENV_PTR     newenv;
    
    
    AM_embedError(newol);
    AM_embedError(newnl);
    AM_heapError(newhtop);
    
    newenv = HN_addNDummyEnv(numabs);
    //opt: the inner susp can be eagerly evaluated for atomic cases; however
    //     it is not implemented because this is a rare case and HN_suspAsEnv
    //     cannot be used here
    /*susp = (DF_TERM_PTR)AM_hreg; //create the explicit susp first
    AM_hreg = (MEM_PTR)DF_MkSusp((DF_TERM_PTR)AM_hreg, newol, newnl,
                                 DF_Deref(DF_LamBody(lamSkp)), newenv);
    */
    susp = HN_suspAsEnv(DF_Deref(DF_LamBody(lamSkp)), newol, newnl, newenv);
    rttp = (DF_TERM_PTR)AM_hreg; //create lam over the susp
    DF_MkLam_((DF_TERM_PTR)AM_hreg, numabs, susp);
    AM_hreg = newhtop;
    
    return rttp;
}

/****************************************************************************/
/* functions for (weak) head normalizing terms of known categories          */
/****************************************************************************/
static DF_TERM_PTR HN_hnormDispatch(DF_TERM_PTR, BOOLEAN whnf);


/* (weak) head normalization on bv or implicit suspension over bv   */
static DF_TERM_PTR HN_hnormBV(DF_TERM_PTR bv, BOOLEAN whnf)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    if (HN_isEmptyEnv()){                        //[|#i, 0, 0, nil|] -> #i
        rttp = bv;
        HN_setRegsRig(bv);
    } else {                                     //[|#i, ol, nl, envlist|] 
        DF_EMBEDLEV dbind = DF_BVIndex(bv);

        if (dbind > ol) {                     //[|#i,ol,nl,e|] -> #i-ol+nl
            DF_PREEMBEDLEV newind = dbind - ol + nl;
            AM_embedError(newind);

            rttp =(DF_TERM_PTR)AM_hreg;
            HN_newBV(newind); 
            HN_setRegsRig(rttp);
            HN_setEmptyEnv();
        } else { // i <= ol
            DF_ENV_PTR   envitem = DF_EnvNth(envlist, dbind);
            DF_EMBEDLEV  nladj = nl-DF_EnvIndex(envitem);//can't have embed err

            if (DF_IsDummy(envitem)){         //[|#i,ol,nl,..@l..|]->#(nl-l)
                //to be removed: for debug
                if (nladj==0) printf("error: [|#i,ol,nl,..@l..|]->#0\n");

                rttp = (DF_TERM_PTR)AM_hreg;
                HN_newBV(nladj); 
                HN_setRegsRig(rttp);
                HN_setEmptyEnv();
            }  else { //DF_IsPairEnv(envitem)
                DF_TERM_PTR tp = DF_Deref(DF_EnvTerm(envitem));

                if ((nladj != 0) && (DF_isSusp(tp))) {
                    DF_PREEMBEDLEV newnl = DF_SuspNL(tp)+nladj;
                    AM_embedError(newnl);
                    //combine suspensions
                    HN_setEnv(DF_SuspOL(tp), newnl, DF_SuspEnv(tp));
                    rttp = HN_hnormDispatch(DF_Deref(DF_SuspTermSkel(tp)),
                                            whnf);
                } else {      // [|#i,ol,nl,..(s,l)..|] -> [|s,0,(nl-l),nil|]
                    HN_setEnv(0, nladj, DF_EMPTYENV); 
                    rttp = HN_hnormDispatch(tp, whnf);
                }
            } //envitem is (s,l)
        } // i<= ol   
    } //non-empty env
    return rttp;
}           

/* (weak) head normalization on abstraction or implicit suspension over
   abstraction.                                                         
   In weak head normalizing implicit suspension, the suspension environment 
   is not explicitly pushed over the abstraction, but left to be performed 
   in the application case on a fly.        
   Note that this is the only case that hnorm terminates with a non-empty 
   environment.                                               
*/
static DF_TERM_PTR HN_hnormLam(DF_TERM_PTR lam, BOOLEAN whnf)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    if (whnf) return rttp = lam; //weak hn 
    else {
        DF_EMBEDLEV numabs = DF_LamEmbedLev(lam);
        DF_TERM_PTR newbody;

        if (HN_isEmptyEnv()){
            newbody = HN_hnormDispatch(DF_Deref(DF_LamBody(lam)), FALSE);
            //to be removed: for debug
            if (!HN_isEmptyEnv()) printf("error: non-empty in hn lam\n");

            return rttp = lam; //body must have been adjusted in place
        } else { 
          //[|lam(n,t),ol,nl,e|] ->lam(n,[|t,ol+n,nl+n,@nl+n-1...::@nl::e|]
            DF_PREEMBEDLEV newol = ol+numabs, newnl = nl+numabs;
            
            AM_embedError(newol);
            AM_embedError(newnl);

            HN_setEnv(newol, newnl, HN_addNDummyEnv(numabs));
            newbody = HN_hnormDispatch(DF_Deref(DF_LamBody(lam)), FALSE);
            //to be removed: for debug
            if (!HN_isEmptyEnv()) printf("error: non-empty env in hn lam\n");

            /* create a new lam on the result of hn the lam body */
            rttp = (DF_TERM_PTR)AM_hreg;
            HN_newLam(newbody, numabs);
        } // non-empty env
        AM_numabs += numabs;
    }// whnf == FALSE
    return rttp;
}
        
/* (weak) head normalization on cons or implicit suspension over cons */
static DF_TERM_PTR HN_hnormCons(DF_TERM_PTR cons, BOOLEAN whnf)
{
    DF_TERM_PTR argvec = DF_ConsArgs(cons),
                rttp; //term pointer to be returned
    if (HN_isEmptyEnv()){
        HN_setRegsCons();
        AM_argvec = argvec;
        AM_numargs = DF_CONSARITY;
        rttp = cons;
    } else {
        BOOLEAN  changed = HN_makeArgvec(argvec, DF_CONSARITY, ol, nl, envlist);
        HN_setRegsCons();
        if (changed){ //new argvec is built because of pushing susp
            rttp = (DF_TERM_PTR)AM_hreg;
            HN_newCons();
        } else rttp = cons;
    }
    return rttp;
} 


/* (weak) head normalization on application or implicit suspension over 
   application. The old application term is destructively changed into
   a reference to its head normal form or its weak head normal form if
   the weak heap normal form is not an implicit suspension (in which
   case the term skeleton must be an abstraction.).
*/
static DF_TERM_PTR HN_hnormApp(DF_TERM_PTR app, BOOLEAN whnf)
{
    DF_TERM_PTR fun = DF_Deref(DF_AppFunc(app)), argvec = DF_AppArgs(app), 
                rttp; // term pointer to be returned
    DF_ARITY    arity = DF_AppArity(app);
    //for book keeping the implicit suspension environment
    DF_EMBEDLEV myol, mynl;
    DF_ENV_PTR  myenvlist;
    BOOLEAN     emptyTopEnv = HN_isEmptyEnv();
    //book keeping the arity before contraction
    DF_ARITY    myarity = arity;

    if (!emptyTopEnv) { //book keeping the current environment
        myol = ol; mynl = nl; myenvlist = envlist;
    }

    fun = HN_hnormDispatch(fun, TRUE); //whf of the function
    //to be removed: for debug
    if (DF_IsRef(fun)) printf("error: reference returned for app function\n");

    while ((arity>0) && (DF_IsLam(fun))) {
        //perform contraction on top-level redexes while you can
        DF_TERM_PTR lamBody = DF_Deref(DF_LamBody(fun)); //abs body
        DF_EMBEDLEV numabsInFun = DF_LamEmbedLev(fun);   
        DF_EMBEDLEV numContract = ((arity<=numabsInFun) ? arity : numabsInFun);
        DF_ENV_PTR  newenv;
        DF_PREEMBEDLEV newol = ol + numContract;
        AM_embedError(newol);

        if (emptyTopEnv) newenv = HN_addNPairEmpEnv(argvec, numContract);
        else newenv = HN_addNPair(argvec, myol, mynl, myenvlist, numContract);
        HN_setEnv(newol, nl, newenv);

        if (arity == numabsInFun){
            fun = HN_hnormDispatch(lamBody, whnf);
            arity = 0;
        } else if (arity > numabsInFun) {
            fun = HN_hnormDispatch(lamBody, TRUE);
            argvec = (DF_TERM_PTR)DF_IncNAtomic((MEM_PTR)argvec, numabsInFun);
            arity -= numabsInFun;
        } else {//arity < numabsInFun
            DF_TERM_PTR newbody = (DF_TERM_PTR)AM_hreg;
            HN_newLam(lamBody, (numabsInFun-arity));
            fun = HN_hnormDispatch(newbody, whnf);
            arity = 0;
        }
        //to be removed: for debug
        if (DF_IsRef(fun)) printf("error: reference returned for app func\n");
    }// while ((arity >0) && (DF_IsLam(fun)))    

    //update or create application
    if (arity == 0) {//app disappears
        rttp = fun;
        if (emptyTopEnv && HN_isEmptyEnv()) HN_updateToRef(app, fun);
    } else {//app persists; Note: now HN_isEmptyEnv must be TRUE
        BOOLEAN changed;
        if (emptyTopEnv) changed = HN_makeArgvecEmpEnv(argvec, arity);
        else changed = HN_makeArgvec(argvec,arity,myol,mynl,myenvlist);
        if ((!changed) && (arity == myarity)) rttp = app;
        else {// create new app and in place update the old
            rttp = (DF_TERM_PTR)AM_hreg;
            HN_newApp();
            if (emptyTopEnv) HN_updateToRef(app, rttp);
        }
    }
    return rttp;
}
      
/* (weak) head normalization on (explicit) suspension or implicit suspension
   with a suspension term skeletion. The explicit suspension is destructivly
   changed to its head normal form or weak head normal form in case
   that the whn is not an implicit susp itself (in which case the term
   skeleton must be an abstraction).
*/ 
static DF_TERM_PTR HN_hnormSusp(DF_TERM_PTR susp, BOOLEAN whnf)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    // for book keeping the env of implicit susp
    DF_EMBEDLEV myol, mynl ;
    DF_ENV_PTR  myenvlist;
    BOOLEAN     emptyTopEnv = HN_isEmptyEnv();
    
    if (!emptyTopEnv){
        myol = ol; mynl = nl; myenvlist = envlist;
    }
    
    //first (weak) head normalize the explicit susp
    HN_setEnv(DF_SuspOL(susp), DF_SuspNL(susp), DF_SuspEnv(susp));
    rttp = HN_hnormDispatch(DF_Deref(DF_SuspTermSkel(susp)), whnf);

    if (emptyTopEnv) {
        if (HN_isEmptyEnv()) HN_updateToRef(susp, rttp);
    } else { // ! emptyTopEnv
        if (HN_isEmptyEnv()) HN_updateToRef(susp, rttp);
        else rttp = HN_pushSuspOverLam(rttp);
        //(weak) head norm the top-level (imp) susp
        HN_setEnv(myol, mynl, myenvlist);
        /* note that AM_numabs, AM_numargs and AM_argvec have to be 
           re-initialized, because the (w)hnf of the inner suspension
           is to be traversed again. */
        HN_initRegs();
        rttp = HN_hnormDispatch(rttp, whnf);
    }
    return rttp;
}

/*******************************************************************/
/* Dispatching on various term categories.                         */
/*******************************************************************/
static DF_TERM_PTR HN_hnormDispatch(DF_TERM_PTR tp, BOOLEAN whnf)
{
    switch (DF_Tag(tp)){
    case DF_TM_TAG_VAR:
    {
        if (!HN_isEmptyEnv()) HN_setEmptyEnv();
        HN_setRegsFlex(tp);
        return tp;
    }
    case DF_TM_TAG_CONST:
    case DF_TM_TAG_INT:
    case DF_TM_TAG_FLOAT:
    case DF_TM_TAG_NIL:
    case DF_TM_TAG_STR:
    case DF_TM_TAG_STREAM:
    {
        if (!HN_isEmptyEnv()) HN_setEmptyEnv();
        HN_setRegsRig(tp);
        return tp;
    }
    case DF_TM_TAG_BVAR:
    {
        return HN_hnormBV(tp, whnf);
    }
    case DF_TM_TAG_CONS:
    {
        return HN_hnormCons(tp, whnf);
    }
    case DF_TM_TAG_LAM:
    {
        return HN_hnormLam(tp, whnf);
    }
    case DF_TM_TAG_APP:
    {
        return HN_hnormApp(tp, whnf);
    }
    case DF_TM_TAG_SUSP:
    {
        return HN_hnormSusp(tp, whnf);
    }
    // to be removed: for debug
    case DF_TM_TAG_REF:
    {
        printf("error: reference in dispatching\n");
        return tp;
    }
    }
}

/**********************************************************************/  
/* the interface routine for head normalization                       */
/**********************************************************************/
void HN_hnorm(DF_TERM_PTR tp)
{
    
    HN_setEmptyEnv();
    HN_initRegs();

    tp = DF_Deref(tp);

    tp = HN_hnormDispatch(tp, FALSE);
}


/*************************************************************************/
/*  (weak) head normalization with occurs-check                          */
/*  Checking is added to CONS and APP cases of the hnorm rountine: in    */
/*  APP case, a checking is made on whether the application is the same  */
/*  as that pointed by the AM_vbbreg register (the actual change is      */
/*  made in the APP case of the dispatching function); in CONS case,     */ 
/*  a checking is made on whether the argvec of the cons term is the same*/
/*  as that pointed by the AM_vbbreg register and the actual change is   */
/*  made in the hnorm function for CONS.                                 */
/*************************************************************************/


/************************************************************************/
/* functions for (weak) head normalizing terms with occurs-check        */
/* of known categories                                                  */
/************************************************************************/
static DF_TERM_PTR HN_hnormDispatchOCC(DF_TERM_PTR, BOOLEAN whnf);


/* (weak) head normalization on bv or implicit suspension over bv   */
static DF_TERM_PTR HN_hnormBVOCC(DF_TERM_PTR bv, BOOLEAN whnf)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    if (HN_isEmptyEnv()){                        //[|#i, 0, 0, nil|] -> #i
        rttp = bv;
        HN_setRegsRig(bv);
    } else {                                     //[|#i, ol, nl, envlist|] 
        DF_EMBEDLEV dbind = DF_BVIndex(bv);

        if (dbind > ol) {                     //[|#i,ol,nl,e|] -> #i-ol+nl
            DF_PREEMBEDLEV newind = dbind - ol + nl;
            AM_embedError(newind);

            rttp =(DF_TERM_PTR)AM_hreg;
            HN_newBV(newind); 
            HN_setRegsRig(rttp);
            HN_setEmptyEnv();
        } else { // i <= ol
            DF_ENV_PTR   envitem = DF_EnvNth(envlist, dbind);
            DF_EMBEDLEV  nladj = nl-DF_EnvIndex(envitem);//can't have embed err

            if (DF_IsDummy(envitem)){         //[|#i,ol,nl,..@l..|]->#(nl-l)
                //to be removed: for debug
                if (nladj==0) printf("error: [|#i,ol,nl,..@l..|]->#0\n");

                rttp = (DF_TERM_PTR)AM_hreg;
                HN_newBV(nladj); 
                HN_setRegsRig(rttp);
                HN_setEmptyEnv();
            }  else { //DF_IsPairEnv(envitem)
                DF_TERM_PTR tp = DF_Deref(DF_EnvTerm(envitem));

                if ((nladj != 0) && (DF_isSusp(tp))) {
                    DF_PREEMBEDLEV newnl = DF_SuspNL(tp)+nladj;
                    AM_embedError(newnl);
                    //combine suspensions
                    HN_setEnv(DF_SuspOL(tp), newnl, DF_SuspEnv(tp));
                    rttp = HN_hnormDispatchOCC(DF_Deref(DF_SuspTermSkel(tp)),
                                               whnf);
                } else {      // [|#i,ol,nl,..(s,l)..|] -> [|s,0,(nl-l),nil|]
                    HN_setEnv(0, nladj, DF_EMPTYENV); 
                    rttp = HN_hnormDispatchOCC(tp, whnf);
                }
            } //envitem is (s,l)
        } // i<= ol   
    } //non-empty env
    return rttp;
}           

/* (weak) head normalization on abstraction or implicit suspension over
   abstraction.                                                         
   In weak head normalizing implicit suspension, the suspension environment 
   is not explicitly pushed over the abstraction, but left to be performed 
   in the application case on a fly.        
   Note that this is the only case that hnorm terminates with a non-empty 
   environment.                                               
*/
static DF_TERM_PTR HN_hnormLamOCC(DF_TERM_PTR lam, BOOLEAN whnf)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    if (whnf) return rttp = lam; //weak hn 
    else {
        DF_EMBEDLEV numabs = DF_LamEmbedLev(lam);
        DF_TERM_PTR newbody;

        if (HN_isEmptyEnv()){
            newbody = HN_hnormDispatchOCC(DF_Deref(DF_LamBody(lam)), FALSE);
            //to be removed: for debug
            if (!HN_isEmptyEnv()) printf("error: non-empty in hn lam\n");

            return rttp = lam; //body must have been adjusted in place
        } else { 
          //[|lam(n,t),ol,nl,e|] ->lam(n,[|t,ol+n,nl+n,@nl+n-1...::@nl::e|]
            DF_PREEMBEDLEV newol = ol+numabs, newnl = nl+numabs;
            
            AM_embedError(newol);
            AM_embedError(newnl);

            HN_setEnv(newol, newnl, HN_addNDummyEnv(numabs));
            newbody = HN_hnormDispatchOCC(DF_Deref(DF_LamBody(lam)), FALSE);
            //to be removed: for debug
            if (!HN_isEmptyEnv()) printf("error: non-empty env in hn lam\n");

            /* create a new lam on the result of hn the lam body */
            rttp = (DF_TERM_PTR)AM_hreg;
            HN_newLam(newbody, numabs);
        } // non-empty env
        AM_numabs += numabs;
    }// whnf == FALSE
    return rttp;
}
        
/* (weak) head normalization on cons or implicit suspension over cons */
static DF_TERM_PTR HN_hnormConsOCC(DF_TERM_PTR cons, BOOLEAN whnf)
{
    DF_TERM_PTR argvec = DF_ConsArgs(cons),
                rttp; //term pointer to be returned
    
    if (AM_vbbreg == argvec) {//occurs-check
        //to add: exception handling
    }

    if (HN_isEmptyEnv()){
        HN_setRegsCons();
        AM_argvec = argvec;
        AM_numargs = DF_CONSARITY;
        rttp = cons;
    } else {
        BOOLEAN  changed = HN_makeArgvec(argvec, DF_CONSARITY, ol, nl, envlist);
        HN_setRegsCons();
        if (changed){ //new argvec is built because of pushing susp
            rttp = (DF_TERM_PTR)AM_hreg;
            HN_newCons();
        } else rttp = cons;
    }
    return rttp;
} 


/* (weak) head normalization on application or implicit suspension over 
   application. The old application term is destructively changed into
   a reference to its head normal form or its weak head normal form if
   the weak heap normal form is not an implicit suspension (in which
   case the term skeleton must be an abstraction.).
*/
static DF_TERM_PTR HN_hnormAppOCC(DF_TERM_PTR app, BOOLEAN whnf)
{
    DF_TERM_PTR fun = DF_Deref(DF_AppFunc(app)), argvec = DF_AppArgs(app), 
                rttp; // term pointer to be returned
    DF_ARITY    arity = DF_AppArity(app);
    //for book keeping the implicit suspension environment
    DF_EMBEDLEV myol, mynl;
    DF_ENV_PTR  myenvlist;
    BOOLEAN     emptyTopEnv = HN_isEmptyEnv();
    //book keeping the arity before contraction
    DF_ARITY    myarity = arity;

    if (!emptyTopEnv) { //book keeping the current environment
        myol = ol; mynl = nl; myenvlist = envlist;
    }

    fun = HN_hnormDispatchOCC(fun, TRUE); //whf of the function
    //to be removed: for debug
    if (DF_IsRef(fun)) printf("error: reference returned for app function\n");

    while ((arity>0) && (DF_IsLam(fun))) {
        //perform contraction on top-level redexes while you can
        DF_TERM_PTR lamBody = DF_Deref(DF_LamBody(fun)); //abs body
        DF_EMBEDLEV numabsInFun = DF_LamEmbedLev(fun);   
        DF_EMBEDLEV numContract = ((arity<=numabsInFun) ? arity : numabsInFun);
        DF_ENV_PTR  newenv;
        DF_PREEMBEDLEV newol = ol + numContract;
        AM_embedError(newol);

        if (emptyTopEnv) newenv = HN_addNPairEmpEnv(argvec, numContract);
        else newenv = HN_addNPair(argvec, myol, mynl, myenvlist, numContract);
        HN_setEnv(newol, nl, newenv);

        if (arity == numabsInFun){
            fun = HN_hnormDispatchOCC(lamBody, whnf);
            arity = 0;
        } else if (arity > numabsInFun) {
            fun = HN_hnormDispatchOCC(lamBody, TRUE);
            argvec = (DF_TERM_PTR)DF_IncNAtomic((MEM_PTR)argvec, numabsInFun);
            arity -= numabsInFun;
        } else {//arity < numabsInFun
            DF_TERM_PTR newbody = (DF_TERM_PTR)AM_hreg;
            HN_newLam(lamBody, (numabsInFun-arity));
            fun = HN_hnormDispatchOCC(newbody, whnf);
            arity = 0;
        }
        //to be removed: for debug
        if (DF_IsRef(fun)) printf("error: reference returned for app func\n");
    }// while ((arity >0) && (DF_IsLam(fun)))
    
    //update or create application
    if (arity == 0) {//app disappears
        rttp = fun;
        if (emptyTopEnv && HN_isEmptyEnv()) HN_updateToRef(app, fun);
    } else {//app persists; Note: now HN_isEmptyEnv must be TRUE
        BOOLEAN changed;
        if (emptyTopEnv) changed = HN_makeArgvecEmpEnv(argvec, arity);
        else changed = HN_makeArgvec(argvec,arity,myol,mynl,myenvlist);
        if ((!changed) && (arity == myarity)) rttp = app;
        else {// create new app and in place update the old
            rttp = (DF_TERM_PTR)AM_hreg;
            HN_newApp();
            if (emptyTopEnv) HN_updateToRef(app, rttp);
        }
    }
    return rttp;
}
      
/* (weak) head normalization on (explicit) suspension or implicit suspension
   with a suspension term skeletion. The explicit suspension is destructivly
   changed to its head normal form or weak head normal form in case
   that the whn is not an implicit susp itself (in which case the term
   skeleton must be an abstraction).
*/ 
static DF_TERM_PTR HN_hnormSuspOCC(DF_TERM_PTR susp, BOOLEAN whnf)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    // for book keeping the env of implicit susp
    DF_EMBEDLEV myol, mynl ;
    DF_ENV_PTR  myenvlist;
    BOOLEAN     emptyTopEnv = HN_isEmptyEnv();
    
    if (!emptyTopEnv){
        myol = ol; mynl = nl; myenvlist = envlist;
    }
    
    //first (weak) head normalize the explicit susp
    HN_setEnv(DF_SuspOL(susp), DF_SuspNL(susp), DF_SuspEnv(susp));
    rttp = HN_hnormDispatchOCC(DF_Deref(DF_SuspTermSkel(susp)), whnf);

    if (emptyTopEnv) {
        if (HN_isEmptyEnv()) HN_updateToRef(susp, rttp);
    } else { // ! emptyTopEnv
        if (HN_isEmptyEnv()) HN_updateToRef(susp, rttp);
        else rttp = HN_pushSuspOverLam(rttp);
        //(weak) head norm the top-level (imp) susp
        HN_setEnv(myol, mynl, myenvlist);
        /*note that AM_numabs, AM_numargs and AM_argvec have to be 
          re-initialized, because the (w)hnf of the inner suspension
          is to be traversed again. */
        HN_initRegs();
        rttp = HN_hnormDispatchOCC(rttp, whnf);
    }

    return rttp;
}

/*******************************************************************/
/* Dispatching on various term categories.                         */
/*******************************************************************/
static DF_TERM_PTR HN_hnormDispatchOCC(DF_TERM_PTR tp, BOOLEAN whnf)
{
    switch (DF_Tag(tp)){
    case DF_TM_TAG_VAR:
    {
        if (!HN_isEmptyEnv()) HN_setEmptyEnv();
        HN_setRegsFlex(tp);
        return tp;
    }
    case DF_TM_TAG_CONST:
    case DF_TM_TAG_INT:
    case DF_TM_TAG_FLOAT:
    case DF_TM_TAG_NIL:
    case DF_TM_TAG_STR:
    case DF_TM_TAG_STREAM:
    {
        if (!HN_isEmptyEnv()) HN_setEmptyEnv();
        HN_setRegsRig(tp);
        return tp;
    }
    case DF_TM_TAG_BVAR:
    {
        return HN_hnormBVOCC(tp, whnf);
    }
    case DF_TM_TAG_CONS:
    {
        return HN_hnormConsOCC(tp, whnf);
    }
    case DF_TM_TAG_LAM:
    {
        return HN_hnormLamOCC(tp, whnf);
    }
    case DF_TM_TAG_APP:
    {
        if (AM_vbbreg == tp) { //occurs-check
            // to add exception handling
        }
        return HN_hnormAppOCC(tp, whnf);
    }
    case DF_TM_TAG_SUSP:
    {
        return HN_hnormSuspOCC(tp, whnf);
    }
    // to be removed: for debug
    case DF_TM_TAG_REF:
    {
        printf("error: reference in dispatching\n");
        return tp;
    }
    }
}

/**********************************************************************/  
/* the interface routine for head normalization with occurs-check     */
/**********************************************************************/
void HN_hnormWithOCC(DF_TERM_PTR tp)
{
    
    HN_setEmptyEnv();
    HN_initRegs();

    tp = DF_Deref(tp);
    tp = HN_hnormDispatchOCC(tp, FALSE);
}


/*********************************************************************/
/* full normalization routines                                       */
/*********************************************************************/
static DF_TERM_PTR HN_lnormDispatch(DF_TERM_PTR, BOOLEAN whnf);

/***************************************************************************/
/* functions for creating argument vectors of applications                 */
/***************************************************************************/

/* normalize implicit suspensions over terms referred by <argvec>. 
   A new argument vector is always created with reference entries to normal 
   forms. New terms may be created during the normalization process.*/
static void HN_redSuspOverArgs(DF_TERM_PTR argvec, DF_ARITY arity,
            DF_EMBEDLEV myol, DF_EMBEDLEV mynl, DF_ENV_PTR myenv)
{
    //book keeping relevant regs. 
    /*Note: no need to book keep current env, since they must be empty when this
      function is invoked; no need to book keep AM_argvec and AM_numargs since
      they are immediately reset by the parent function. */
    DF_TERM_PTR head     = AM_head;
    DF_EMBEDLEV numabs   = AM_numabs;
    FLAG        rigflag  = AM_rigflag;
    FLAG        consflag = AM_consflag;

    DF_TERM_PTR arg; //one argument in argvec
    DF_TERM_PTR myargvec = (DF_TERM_PTR)AM_hreg; //new argvec
    MEM_PTR     newhtop = DF_IncNAtomic(AM_hreg, arity);
    int i;
   
    AM_heapError(newhtop);
    AM_hreg = newhtop; //arrange heap top for creating terms in norm args

    for (i = 1; i <= arity; i++){
        arg = DF_Deref(argvec); //skp
        HN_setEnv(myol, mynl, myenv); //imp susp environment        
        HN_initRegs();
        myargvec = DF_MkRef(myargvec, HN_lnormDispatch(arg, FALSE));
        argvec = (DF_TERM_PTR)DF_IncAtomic((MEM_PTR)argvec);
    }
    //reset registers. Again, there is no need to reset env to empty, since
    //normal forms of arguments must have empty env.
    AM_numabs = numabs; AM_head = head; 
    AM_rigflag = rigflag; AM_consflag = consflag;
}

/* normalize terms referred by <argvec>. Top level redexes in such terms 
   must have been in place updated (since they are strong normalized under
   an empty environment),
   and therefore the old argument vector, <argvec>, is reused. New terms
   may be created during the normalization process. */
static void HN_redArgs(DF_TERM_PTR argvec, DF_ARITY arity)
{
    int i;
    //book keeping relevant regs
    DF_TERM_PTR head     = AM_head;
    DF_EMBEDLEV numabs   = AM_numabs;
    FLAG        rigflag  = AM_rigflag;
    FLAG        consflag = AM_consflag;
    
    DF_TERM_PTR arg; //one argument in argvec
    
    for (i = 1; i <= arity; i++){
        arg = DF_Deref(argvec);
        /*no need to set empty env, since normal forms must have empty env*/ 
        HN_initRegs();
        arg = HN_lnormDispatch(arg, FALSE);
        argvec = (DF_TERM_PTR)DF_IncAtomic((MEM_PTR)argvec);
    }
    //reset registers
    AM_numabs = numabs; AM_head = head; 
    AM_rigflag = rigflag; AM_consflag = consflag;
}    

/* arrange a new argument vector for the normal form of a implicit susp 
   over application. 
   Nested applications are unfolded first if necessary, and then the 
   implicit susps over arguments are normalized. A new argument vector
   is always created.
*/
static BOOLEAN HN_redArgvec(DF_TERM_PTR argvec, DF_ARITY arity,DF_EMBEDLEV myol,
                            DF_EMBEDLEV mynl, DF_ENV_PTR myenv)
{
    DF_TERM_PTR newargvec = (DF_TERM_PTR)AM_hreg; //new argvec
    DF_PREARITY newarity;

    if (AM_numargs != 0){//unfold nested app first when necessary
        MEM_PTR newhtop  = DF_IncNAtomic(AM_hreg, AM_numargs);
        AM_heapError(newhtop);

        newarity = arity + AM_numargs;
        AM_arityError(newarity);
        
        HN_copyArgs(AM_argvec, AM_numargs);//layout nested args
    } else newarity = arity;
    //normalize arguments
    HN_redSuspOverArgs(argvec, arity, myol, mynl, myenv);
    AM_argvec = newargvec;
    AM_numargs = newarity;
    return TRUE;
}

/* arrange argument vector for the normal form of an application.
   If nested applications are unfolded, new argument vector is created.
   Otherwise, the old one (<argvec>) is reused, since the arguments 
   are in place updated to their normal forms. The flag <changed> is used
   to denote whether old argvec is reused.*/

static BOOLEAN HN_redArgvecEmpEnv(DF_TERM_PTR argvec, DF_ARITY arity)
{
    BOOLEAN changed; //flag denoting if new argvec is to be used
    if (AM_numargs != 0){//unfold nested app first when necessary
        DF_PREARITY newarity  = arity + AM_numargs;
        DF_TERM_PTR newargvec = (DF_TERM_PTR)AM_hreg;
        MEM_PTR     newhtop   = DF_IncNAtomic(AM_hreg, AM_numargs);

        AM_heapError(newhtop);
        AM_arityError(newarity);
        
        HN_copyArgs(AM_argvec, AM_numargs);//layout nested args
        HN_redSuspOverArgs(argvec, arity, 0, 0, DF_EMPTYENV);//created new arg
        
        AM_argvec   = newargvec;
        AM_numargs  = newarity;
        changed     = TRUE;
    } else {
        HN_redArgs(argvec, arity);//normalization with destructive changes
        
        AM_argvec  = argvec;
        AM_numargs = arity;
        changed    = FALSE;
    }
    
    return changed;
}


/****************************************************************************/
/* functions for fully normalizing terms of known categories                */
/****************************************************************************/

/* full normalization on bv or implicit suspension over bv   */
static DF_TERM_PTR HN_lnormBV(DF_TERM_PTR bv, BOOLEAN whnf)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    if (HN_isEmptyEnv()){                        //[|#i, 0, 0, nil|] -> #i
        rttp = bv;
        HN_setRegsRig(bv);
    } else {                                     //[|#i, ol, nl, envlist|] 
        DF_EMBEDLEV dbind = DF_BVIndex(bv);

        if (dbind > ol) {                     //[|#i,ol,nl,e|] -> #i-ol+nl
            DF_PREEMBEDLEV newind = dbind - ol + nl;
            AM_embedError(newind);
            rttp =(DF_TERM_PTR)AM_hreg;
            HN_newBV(newind); 
            HN_setRegsRig(rttp);
            HN_setEmptyEnv();
        } else { // i <= ol
            DF_ENV_PTR   envitem = DF_EnvNth(envlist, dbind);
            DF_EMBEDLEV  nladj = nl-DF_EnvIndex(envitem);//can't have embed err

            if (DF_IsDummy(envitem)){         //[|#i,ol,nl,..@l..|]->#(nl-l)
                //to be removed: for debug
                if (nladj==0) printf("error: [|#i,ol,nl,..@l..|]->#0\n");
                
                rttp = (DF_TERM_PTR)AM_hreg;
                HN_newBV(nladj); 
                HN_setRegsRig(rttp);
                HN_setEmptyEnv();
            }  else { //DF_IsPairEnv(envitem)
                DF_TERM_PTR tp = DF_Deref(DF_EnvTerm(envitem));

                if ((nladj != 0) && (DF_isSusp(tp))) {
                    DF_PREEMBEDLEV newnl = DF_SuspNL(tp)+nladj;
                    AM_embedError(newnl);
                    //combine suspensions
                    HN_setEnv(DF_SuspOL(tp), newnl, DF_SuspEnv(tp));
                    rttp = HN_lnormDispatch(DF_Deref(DF_SuspTermSkel(tp)),
                                            whnf);
                } else {      // [|#i,ol,nl,..(s,l)..|] -> [|s,0,(nl-l),nil|]
                    HN_setEnv(0, nladj, DF_EMPTYENV); 
                    rttp = HN_lnormDispatch(tp, whnf);
                }
            } //envitem is (s,l)
        } // i<= ol   
    } //non-empty env
    return rttp;
}           

/* full normalization on abstraction or implicit suspension over
   abstraction.                                                         
   In weak head normalizing implicit suspension, the suspension environment 
   is not explicitly pushed over the abstraction, but left to be performed 
   in the application case on a fly.        
*/
static DF_TERM_PTR HN_lnormLam(DF_TERM_PTR lam, BOOLEAN whnf)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    if (whnf) return rttp = lam; //weak hn 
    else {
        DF_EMBEDLEV numabs = DF_LamEmbedLev(lam);
        DF_TERM_PTR newbody;

        if (HN_isEmptyEnv()){
            newbody = HN_lnormDispatch(DF_Deref(DF_LamBody(lam)), FALSE);
            //to be removed: for debug
            if (!HN_isEmptyEnv()) printf("error: non-empty in hn lam\n");

            return rttp = lam; //body must have been adjusted in place
        } else { 
          //[|lam(n,t),ol,nl,e|] ->lam(n,[|t,ol+n,nl+n,@nl+n-1...::@nl::e|]
            DF_PREEMBEDLEV newol = ol+numabs, newnl = nl+numabs;
            
            AM_embedError(newol);
            AM_embedError(newnl);

            HN_setEnv(newol, newnl, HN_addNDummyEnv(numabs));
            newbody = HN_lnormDispatch(DF_Deref(DF_LamBody(lam)), FALSE);
            //to be removed: for debug
            if (!HN_isEmptyEnv()) printf("error: non-empty env in hn lam\n");

            /* create a new lam on the result of hn the lam body */
            rttp = (DF_TERM_PTR)AM_hreg;
            HN_newLam(newbody, numabs);
        } // non-empty env
        AM_numabs += numabs;
    }// whnf == FALSE
    return rttp;
}

/* full normalization on cons or implicit suspension over cons */
static DF_TERM_PTR HN_lnormCons(DF_TERM_PTR cons, BOOLEAN whnf)
{
    DF_TERM_PTR argvec = DF_ConsArgs(cons),
                rttp; //term pointer to be returned
    if (HN_isEmptyEnv()){
        HN_redArgs(argvec, DF_CONSARITY);
        HN_setRegsCons();
        AM_argvec = argvec;
        AM_numargs = DF_CONSARITY;
        rttp = cons;
    } else {
        DF_TERM_PTR newarg = (DF_TERM_PTR)AM_hreg;
        HN_redSuspOverArgs(argvec, DF_CONSARITY, ol, nl, envlist);
        HN_setRegsCons();
        AM_argvec = newarg;
        AM_numargs = DF_CONSARITY;
        //new argvec is always used because of normalizing susp args
        rttp = (DF_TERM_PTR)AM_hreg;
        HN_newCons();
    }
    return rttp;
} 

/* full normalization on application or implicit suspension over 
   application. The old application term is destructively changed into
   a reference to its normal form or its weak head normal form if
   the weak head normal form is not an implicit suspension (in which
   case the term skeleton must be an abstraction.).
*/
static DF_TERM_PTR HN_lnormApp(DF_TERM_PTR app, BOOLEAN whnf)
{
    DF_TERM_PTR fun = DF_Deref(DF_AppFunc(app)), argvec = DF_AppArgs(app), 
                rttp; // term pointer to be returned
    DF_ARITY    arity = DF_AppArity(app);
    //for book keeping the implicit suspension environment
    DF_EMBEDLEV myol, mynl;
    DF_ENV_PTR  myenvlist;
    BOOLEAN     emptyTopEnv = HN_isEmptyEnv();
    //book keeping the arity before contraction
    DF_ARITY    myarity = arity;

    if (!emptyTopEnv) { //book keeping the current environment
        myol = ol; mynl = nl; myenvlist = envlist;
    }

    fun = HN_lnormDispatch(fun, TRUE); //whf of the function
    //to be removed: for debug
    if (DF_IsRef(fun)) printf("error: reference returned for app function\n");

    while ((arity>0) && (DF_IsLam(fun))) {
        //perform contraction on top-level redexes while you can
        DF_TERM_PTR lamBody = DF_Deref(DF_LamBody(fun)); //abs body
        DF_EMBEDLEV numabsInFun = DF_LamEmbedLev(fun);   
        DF_EMBEDLEV numContract = ((arity<=numabsInFun) ? arity : numabsInFun);
        DF_ENV_PTR  newenv;
        DF_PREEMBEDLEV newol = ol + numContract;
        AM_embedError(newol);

        if (emptyTopEnv) newenv = HN_addNPairEmpEnv(argvec, numContract);
        else newenv = HN_addNPair(argvec, myol, mynl, myenvlist, numContract);
        HN_setEnv(newol, nl, newenv);

        if (arity == numabsInFun){
            fun = HN_lnormDispatch(lamBody, whnf);
            arity = 0;
        } else if (arity > numabsInFun) {
            fun = HN_lnormDispatch(lamBody, TRUE);
            argvec = (DF_TERM_PTR)DF_IncNAtomic((MEM_PTR)argvec, numabsInFun);
            arity -= numabsInFun;
        } else {//arity < numabsInFun
            DF_TERM_PTR newbody = (DF_TERM_PTR)AM_hreg;
            HN_newLam(lamBody, (numabsInFun-arity));
            fun = HN_lnormDispatch(newbody, whnf);
            arity = 0;
        }
        //to be removed: for debug
        if (DF_IsRef(fun)) printf("error: reference returned for app func\n");
    }// while ((arity >0) && (DF_IsLam(fun)))
    
    //update or create application
    if (arity == 0) {//app disappears
        rttp = fun;
        if (emptyTopEnv && HN_isEmptyEnv()) HN_updateToRef(app, fun);
    } else {//app persists; Note: now HN_isEmptyEnv must be TRUE
        BOOLEAN changed;
        if (emptyTopEnv) changed = HN_redArgvecEmpEnv(argvec, arity);
        else changed = HN_redArgvec(argvec,arity,myol,mynl,myenvlist);
                
        if ((!changed) && (arity == myarity)) rttp = app;
        else {// create new app and in place update the old
            rttp = (DF_TERM_PTR)AM_hreg;
            HN_newApp();
            if (emptyTopEnv) HN_updateToRef(app, rttp);
        }
    }
    return rttp;
}

/* full normalization on (explicit) suspension or implicit suspension
   with a suspension term skeletion. The explicit suspension is destructivly
   changed to its head normal form or weak head normal form in case
   that the whn is not an implicit susp itself (in which case the term
   skeleton must be an abstraction).
*/ 
static DF_TERM_PTR HN_lnormSusp(DF_TERM_PTR susp, BOOLEAN whnf)
{
    DF_TERM_PTR rttp; //term pointer to be returned
    // for book keeping the env of implicit susp
    DF_EMBEDLEV myol, mynl ;
    DF_ENV_PTR  myenvlist;
    
    if (HN_isEmptyEnv()){ //normlizing exp susp
        HN_setEnv(DF_SuspOL(susp), DF_SuspNL(susp), DF_SuspEnv(susp));
        rttp = HN_lnormDispatch(DF_Deref(DF_SuspTermSkel(susp)), whnf);
        if (HN_isEmptyEnv()) HN_updateToRef(susp, rttp);
    } else {//nested susp
        //book keeping top env
        myol = ol; mynl = nl; myenvlist = envlist;
        //(weak) head normalizing exp susp
        HN_setEnv(DF_SuspOL(susp), DF_SuspNL(susp), DF_SuspEnv(susp));
        rttp = HN_hnormDispatch(DF_Deref(DF_SuspTermSkel(susp)), whnf);
        PR_PrintTerm(rttp);
        printf("\n");

        if (HN_isEmptyEnv()) HN_updateToRef(susp, rttp);
        else rttp = HN_pushSuspOverLam(rttp);

        HN_setEnv(myol, mynl, myenvlist);
        printf("ol:%d, nl:%d, envlist:",ol, nl);
        PR_PrintEnv(envlist);
        printf("\n");
        
        /* note that AM_numabs, AM_numargs and AM_argvec have to be 
           re-initialized, because the (w)hnf of the inner suspension
           is to be traversed again. */
        HN_initRegs();
        rttp = HN_lnormDispatch(rttp, whnf);
    }
    return rttp;
}


/*******************************************************************/
/* Dispatching on various term categories.                         */
/*******************************************************************/
static DF_TERM_PTR HN_lnormDispatch(DF_TERM_PTR tp, BOOLEAN whnf)
{
    switch (DF_Tag(tp)){
    case DF_TM_TAG_VAR:
    {
        if (!HN_isEmptyEnv()) HN_setEmptyEnv();
        HN_setRegsFlex(tp);
        return tp;
    }
    case DF_TM_TAG_CONST:
    case DF_TM_TAG_INT:
    case DF_TM_TAG_FLOAT:
    case DF_TM_TAG_NIL:
    case DF_TM_TAG_STR:
    case DF_TM_TAG_STREAM:
    {
        if (!HN_isEmptyEnv()) HN_setEmptyEnv();
        HN_setRegsRig(tp);
        return tp;
    }
    case DF_TM_TAG_BVAR:
    {
        return HN_lnormBV(tp, whnf);
    }
    case DF_TM_TAG_CONS:
    {
        return HN_lnormCons(tp, whnf);
    }
    case DF_TM_TAG_LAM:
    {
        return HN_lnormLam(tp, whnf);
    }
    case DF_TM_TAG_APP:
    {
        return HN_lnormApp(tp, whnf);
    }
    case DF_TM_TAG_SUSP:
    {
        return HN_lnormSusp(tp, whnf);
    }
    // to be removed: for debug
    case DF_TM_TAG_REF:
    {
        printf("error: reference in dispatching\n");
        return tp;
    }
    }
}

/***********************************************************************/
/* the interface routine for full normalization                        */
/***********************************************************************/
void HN_lnorm(DF_TERM_PTR tp)
{
    HN_setEmptyEnv();
    HN_initRegs();
    
    tp = DF_Deref(tp);
    tp = HN_lnormDispatch(tp, FALSE);
}

#endif //HNORM_C
