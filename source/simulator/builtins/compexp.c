/****************************************************************************
 *                                                                          *
 * File compexp.h{c}                                                        * 
 *                                                                          *
 * Evaluating comparison:                                                   *
 * term1 <comp op> term2                                                    *
 *                                                                          *
 * The pointers to term1, term2 are in REG(1) and REG(2).                   *
 *                                                                          *
 * Approach:                                                                *
 * We don't want to repeat work already done in BIEVAL_eval(), and we don't *
 * want to maintain two versions of evaluation.  The implementation of "is" *
 * provides three functions (evalInt, evalFloat, evalString)                *
 * which are used in the current routine.                                   *
 ****************************************************************************/
#include "compexp.h"
#include "evalexp.h"
#include "builtins.h"
#include "../abstmachine.h"  //to be modified
#include "../dataformats.h"  //to be modified
#include "../mcstring.h"     //to be modified
#include "../../system/error.h" //to be modified

void BICOMP_comp()
{
    int success;
    DF_TermPtr lOp, rOp;

    lOp = (DF_TermPtr)AM_reg(1);
    rOp = (DF_TermPtr)AM_reg(2);
    
    switch (BI_number){
    case BI_INT_GE: 
    { success = BIEVAL_evalInt(lOp) >= BIEVAL_evalInt(rOp); break; }
    case BI_INT_GT:
    { success = BIEVAL_evalInt(lOp) >  BIEVAL_evalInt(rOp); break; }
    case BI_INT_LE:
    { success = BIEVAL_evalInt(lOp) <= BIEVAL_evalInt(rOp); break; }        
    case BI_INT_LT:
    { success = BIEVAL_evalInt(lOp) <  BIEVAL_evalInt(rOp); break; }
    case BI_FLOAT_GE:
    { success = BIEVAL_evalFloat(lOp) >= BIEVAL_evalFloat(rOp); break; }
    case BI_FLOAT_GT:
    { success = BIEVAL_evalFloat(lOp) >  BIEVAL_evalFloat(rOp); break; }
    case BI_FLOAT_LE:
    { success = BIEVAL_evalFloat(lOp) <= BIEVAL_evalFloat(rOp); break; }
    case BI_FLOAT_LT:
    { success = BIEVAL_evalFloat(lOp) <  BIEVAL_evalFloat(rOp); break; }
    case BI_STR_GE:
    { success = (MCSTR_compareStrs(DF_strDataValue(BIEVAL_evalStr(lOp)),
                                   DF_strDataValue(BIEVAL_evalStr(rOp))) >= 0);
      break;
    }
    case BI_STR_GT:
    { success = (MCSTR_compareStrs(DF_strDataValue(BIEVAL_evalStr(lOp)),
                                   DF_strDataValue(BIEVAL_evalStr(rOp))) > 0);
      break;
    }
    case BI_STR_LE:
    { success = (MCSTR_compareStrs(DF_strDataValue(BIEVAL_evalStr(lOp)),
                                   DF_strDataValue(BIEVAL_evalStr(rOp))) <= 0);
      break;
    }
    case BI_STR_LT:
    { success = (MCSTR_compareStrs(DF_strDataValue(BIEVAL_evalStr(lOp)),
                                   DF_strDataValue(BIEVAL_evalStr(rOp))) >= 0);
      break;
    }
    }
    
    if (success) AM_preg = AM_cpreg;
    else EM_THROW(EM_FAIL);
}

