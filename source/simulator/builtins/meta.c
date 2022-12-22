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

/****************************************************************************/
/*                                                                          */
/*                                                                          */
/* builtins/meta.c defines the builtin BIMETA_solve which is used to solve a*/
/* term as a query and BIMETA_not which is used to realize                  */
/* negation-by-failure                                                      */
/*                                                                          */
/****************************************************************************/
#include <stdio.h>            //temp
#include "../printterm.h"
//#include "../print.h"
#include "../../system/stream.h"

#include "meta.h"
#include "builtins.h"
#include "../dataformats.h"   
#include "../abstmachine.h"   
#include "../hnorm.h"         
#include "../../tables/pervasives.h" 
#include "../../tables/instructions.h" 
#include "../../system/error.h"      


/****************************************************************************/
/* Auxiliary functions for BIMETA_solve();                                  */
/* The decomposition of the top-level term being processed is assumed to    */
/* be stored in the registers AM_head, AM_argVec and AM_numargs.            */
/****************************************************************************/
static void BIMETA_solveAnd()
{
    MemPtr ep = AM_findtosEnv() + AM_ENV_FIX_SIZE;
    AM_DataTypePtr var;

    AM_stackError(ep + AM_DATA_SIZE * 3);           //3 env vars
    AM_ereg = AM_mkEnvWOUC(ep);                     //create env rec
    DF_mkRef((MemPtr)AM_envVar(1), (AM_argVec + 1));//set env vars 
    var =  AM_envVar(2);
    *((MemPtr *)var) = AM_b0reg;                    
    AM_cpreg = AM_andCode;                          
    DF_copyAtomic(AM_argVec, (MemPtr)(AM_reg(1)));
    AM_preg  = AM_solveCode;
}

static void BIMETA_solveOr()
{
    MemPtr cp;
                                                    //one register to be saved
    AM_tosreg = (MemPtr)((AM_DataTypePtr)(AM_findtosEnv() + AM_CP_FIX_SIZE)+1);
    AM_stackError(AM_tosreg);
    cp = AM_tosreg - 1;
    
    AM_saveStateCP(cp, AM_orCode);
    DF_mkRef((MemPtr)AM_cpArg(cp, 1), (AM_argVec + 1));
    AM_b0reg = AM_breg = cp;
    AM_hbreg = AM_hreg;

    DF_copyAtomic(AM_argVec, (MemPtr)(AM_reg(1)));
    AM_preg = AM_solveCode;
}

static void BIMETA_solveSome()
{
    MemPtr varPtr = AM_hreg + DF_TM_APP_SIZE;
    MemPtr nhreg  = varPtr  + DF_TM_ATOMIC_SIZE;
    
    AM_heapError(nhreg);
    DF_mkApp(AM_hreg, 1, AM_argVec, (DF_TermPtr)varPtr);
    DF_mkVar(varPtr, AM_ucreg);
    DF_mkRef((MemPtr)AM_reg(1), (DF_TermPtr)AM_hreg);
    
    AM_hreg = nhreg;
    AM_preg = AM_solveCode;
}

static void BIMETA_solveAll()
{
    MemPtr ep = AM_findtosEnv() + AM_ENV_FIX_SIZE;
    MemPtr constPtr = AM_hreg + DF_TM_APP_SIZE;
    MemPtr nhreg    = constPtr + DF_TM_ATOMIC_SIZE;
    
    AM_stackError(ep + AM_DATA_SIZE); //no env vars
    AM_ereg = AM_mkEnvWOUC(ep);
    AM_cpreg = AM_allCode;
    
    AM_heapError(nhreg); 
    DF_mkApp(AM_hreg, 1, AM_argVec, (DF_TermPtr)constPtr);
    AM_ucError(AM_ucreg);
    AM_ucreg++;
    DF_mkConst(constPtr, AM_ucreg, PERV_UNIV_INDEX);
    DF_mkRef((MemPtr)AM_reg(1), (DF_TermPtr)AM_hreg);

    AM_hreg = nhreg;
    AM_preg = AM_solveCode;
}

static void BIMETA_setRegs(int pred)
{
    int i, j;    
    for (i = 1; i <= AM_numArgs; i++)
        DF_mkRef((MemPtr)(AM_reg(i)), (AM_argVec+i-1));

    j = AM_cstTyEnvSize(pred);
    if (j > 0) {
        DF_TypePtr tyPtr = DF_constType(AM_head);
        do {
            DF_copyAtomicType(tyPtr, (MemPtr)(AM_reg(i)));
            tyPtr++;
            j--;
            i++;
        } while (j > 0);    
    }
}

/************************************************************/
/*             BIMETA_solve()                               */
/************************************************************/
void BIMETA_solve()
{   
    DF_TermPtr tmPtr = (DF_TermPtr)AM_reg(1);
    HN_hnorm(tmPtr);

    if (AM_rigFlag) { //rigid term (must be const head); assured by type (o) 
        int pred = DF_constTabIndex(AM_head);
        if (PERV_isLogicSymb(pred)) {
            switch (PERV_logicSymb(pred)) {
            case PERV_AND: case PERV_AMPAND: { BIMETA_solveAnd();     break; }
            case PERV_OR :   {BIMETA_solveOr();                       break; }
            case PERV_SOME:  {BIMETA_solveSome();                     break; }
            case PERV_ALL:   {BIMETA_solveAll();                      break; }
            case PERV_L_TRUE:{AM_preg = AM_cpreg;                     break; }
            case PERV_CUT:   {AM_breg = AM_b0reg; AM_preg = AM_cpreg; break; }
            case PERV_FAIL:  {EM_THROW(EM_FAIL);                             }
            case PERV_HALT:  {AM_preg = AM_haltCode;                  break; }
            case PERV_STOP:  {AM_preg = AM_stopCode;                  break; }
            }
        } else { //pred is not a logic symbol
            if (PERV_isPredSymb(pred)) { //head is a builtin predicate symbol
                BIMETA_setRegs(pred); //set up term and type registers
                AM_preg = AM_builtinCode;
                *((INSTR_OneByteInt*)(AM_preg + INSTR_I1X_I1)) =
                    PERV_predBuiltin(pred);
            } else { //head is a constant symbol                
                MemPtr    ip;
                CSpacePtr cl;
                AM_findCode(pred, &cl, &ip);                
                if (cl) {
                    BIMETA_setRegs(pred); //set up term and type registers
                    AM_b0reg = AM_breg;
                    AM_preg  = cl;
                    AM_cireg = ip;
                    if (AM_isImplCI()) AM_cereg = AM_cimpCE();
                } else EM_THROW(EM_FAIL);
            }       //head is a constant symbol
        }       //pred is not a logic symbol
    } else EM_error(BI_ERROR_FLEX_GOAL); //flex term
}

/************************************************************/
/*             BIMETA_not()                                 */
/************************************************************/
void BIMETA_not()
{
    MemPtr cp;
                                                    //one register to be saved
    AM_tosreg = AM_findtosEnv() + AM_CP_FIX_SIZE;
    AM_stackError(AM_tosreg);
    cp = AM_tosreg - 1;
    
    AM_saveStateCP(cp, AM_notCode2);
    AM_b0reg = AM_breg;
    AM_breg  = cp;
    AM_hbreg = AM_hreg;

    AM_preg = AM_notCode1;
}
/************************************************************/
/*             BIMETA_unify()                               */
/************************************************************/
void BIMETA_unify()
{
    AM_preg = AM_eqCode;
}



