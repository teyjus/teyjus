//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                              //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify            //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                 //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.           //
//////////////////////////////////////////////////////////////////////////////

/*****************************************************************************/
/*                                                                           */
/* builtins/evalexp.{c,h}                                                    */
/*****************************************************************************/
#include <stdio.h> 
#include <string.h>
#include <math.h>

#include "evalexp.h"
#include "builtins.h"
#include "../mctypes.h"     
#include "../mcstring.h"    
#include "../dataformats.h" 
#include "../hnorm.h"       
#include "../abstmachine.h" 
#include "../../tables/pervasives.h" 
#include "../../system/error.h"      


/************************************************************************
 *                                                                      *
 * The main procedure that is provided to the external world is         *
 *    BI_eval()                                                         *
 * which implements the "is" builtin.                                   *
 * "is" has the following polymorphic type                              *
 *    is: A -> A -> o                                                   *
 * Semantics of "is":                                                   *
 *    (X is V) : evaluates V then bind X to V.                          *
 * The polymorphism is an ad hoc one.                                   *
 * Currently the type of V must be one  of int, float, or string.       *
 * The arguments to "is" are passed in the following way:               *
 *    REG(1) stores the term X                                          *
 *    REG(2) stores the term V                                          *
 *    REG(3) stores the type environment of it                          *
 * The actual evaluation is isolated in three procedures:               *
 *    eval_int_aux, eval_float_aux, and eval_string_aux.                *
 * These are reused in implementing comparison builtins.                *
 *                                                                      *
 * Put "V" on the slstack, call eval_<>_aux() to evaluate V, put V      *
 * in REG(2) and set the program counter to code that implements =.     *
 *                                                                      *
 ************************************************************************/

void BIEVAL_eval()
{
    DF_TermPtr tmPtr = (DF_TermPtr)AM_reg(2);
    DF_TypePtr tyPtr = DF_typeDeref((DF_TypePtr)AM_reg(3));    
    switch (DF_typeKindTabIndex(tyPtr)) {
    case PERV_INT_INDEX: 
    {  DF_mkInt((MemPtr)AM_reg(2), BIEVAL_evalInt(tmPtr));     break; }
    case PERV_REAL_INDEX:
    {  DF_mkFloat((MemPtr)AM_reg(2), BIEVAL_evalFloat(tmPtr)); break; }
    case PERV_STRING_INDEX:
    {  DF_mkStr((MemPtr)AM_reg(2), BIEVAL_evalStr(tmPtr));     break; }
    default:
    {  EM_error(BI_ERROR_EVAL_TYPE);                           break; }
    }
    //binding X to V
    AM_preg = AM_eqCode;
    return;
}


/*************************************************************************
 *                                                                       *
 *  evaluation on expressions of integer type                            *
 *                                                                       *
 *************************************************************************/
int BIEVAL_evalInt(DF_TermPtr tmPtr)
{
    DF_TermPtr mytmPtr;
    HN_hnorm(tmPtr);
    mytmPtr = DF_termDeref(tmPtr);
    if (DF_isAtomic(mytmPtr)) {
        switch (DF_termTag(AM_head)) {
        case DF_TM_TAG_INT: return DF_intValue(AM_head); 
        case DF_TM_TAG_VAR: EM_error(BI_ERROR_FLEX_HEAD, mytmPtr);
        default:            EM_error(BI_ERROR_ILLEGAL_ARG, mytmPtr);
        }
    } else { //non atomic term
        if (DF_isApp(mytmPtr)) {
            DF_TermPtr argVec = AM_argVec;
            int        cstInd;
            int        ival1, ival2;
            double     fval;
            
            if (!AM_rigFlag) EM_error(BI_ERROR_FLEX_HEAD, mytmPtr); 
            
            cstInd = DF_constTabIndex(AM_head);
            switch(cstInd){
            case PERV_INTUMINUS_INDEX :
                return -BIEVAL_evalInt(argVec);
            case PERV_INTPLUS_INDEX :
                return BIEVAL_evalInt(argVec) + BIEVAL_evalInt(argVec+1);
            case PERV_INTMINUS_INDEX :
                return BIEVAL_evalInt(argVec) - BIEVAL_evalInt(argVec+1);
            case PERV_INTMULT_INDEX :
                return BIEVAL_evalInt(argVec) * BIEVAL_evalInt(argVec+1);
            case PERV_INTDIV_INDEX :
                ival1 = BIEVAL_evalInt(argVec);
                ival2 = BIEVAL_evalInt(argVec+1);
                if (ival2 == 0) EM_error(BI_ERROR_DIV_BY_ZERO);
                return ival1 / ival2;
            case PERV_MOD_INDEX :
                return BIEVAL_evalInt(argVec) % BIEVAL_evalInt(argVec+1);
            case PERV_IABS_INDEX:
                ival1 = BIEVAL_evalInt(argVec);
                return (ival1 < 0 ? -ival1 : ival1);
            case PERV_FLOOR_INDEX:
                return (int)floor((double)BIEVAL_evalFloat(argVec));
            case PERV_CEIL_INDEX:
                return (int)ceil((double)BIEVAL_evalFloat(argVec));
            case PERV_TRUNC_INDEX:
                fval = (double)BIEVAL_evalFloat(argVec);
                return (int)(fval > 0 ? floor(fval) : ceil(fval));
            case PERV_STOI_INDEX:
                return MCSTR_ord(DF_strDataValue(BIEVAL_evalStr(argVec)));
            case PERV_SLEN_INDEX:
                return MCSTR_strLength(DF_strDataValue(BIEVAL_evalStr(argVec)));
            default: EM_error(BI_ERROR_CONST_IND, cstInd);
            } //switch
        } else EM_error(BI_ERROR_ILLEGAL_ARG, mytmPtr);
    } //non atomic

    //Impossible to reach this point.
    return 0;

}

/*************************************************************************
 *                                                                       *
 *  evaluation on expressions of float type                              *
 *                                                                       *
 *************************************************************************/
float BIEVAL_evalFloat(DF_TermPtr tmPtr)
{
    DF_TermPtr mytmPtr;
    HN_hnorm(tmPtr);
    mytmPtr = DF_termDeref(tmPtr);
    if (DF_isAtomic(mytmPtr)) {
        switch (DF_termTag(AM_head)) {
        case DF_TM_TAG_FLOAT: return DF_floatValue(AM_head); 
        case DF_TM_TAG_VAR:   EM_error(BI_ERROR_FLEX_HEAD, mytmPtr);
        default:              EM_error(BI_ERROR_ILLEGAL_ARG, mytmPtr);
        }
    } else { //non atomic term
        if (DF_isApp(mytmPtr)) {
            DF_TermPtr argVec = AM_argVec;
            int        cstInd;
            float      fval1, fval2;

            if (!AM_rigFlag)  EM_error(BI_ERROR_FLEX_HEAD, mytmPtr);

            cstInd = DF_constTabIndex(AM_head);
            switch(cstInd){
            case PERV_REALUMINUS_INDEX:    // unary minus on real 
                return -BIEVAL_evalFloat(argVec);
            case PERV_REALMINUS_INDEX:     // subtraction on reals
                return BIEVAL_evalFloat(argVec) - 
                    BIEVAL_evalFloat(argVec+1);
            case PERV_REALPLUS_INDEX :     // addition on reals
                return BIEVAL_evalFloat(argVec) +
                    BIEVAL_evalFloat(argVec+1);
            case PERV_REALMULT_INDEX :     // multiplication on reals
                return BIEVAL_evalFloat(argVec) *
                    BIEVAL_evalFloat(argVec+1);
            case PERV_REALDIV_INDEX :      // division
                fval1 = BIEVAL_evalFloat(argVec);
                fval2 = BIEVAL_evalFloat(argVec+1);
                if (fval2 == 0.0) EM_error(BI_ERROR_DIV_BY_ZERO);
                return fval1 / fval2;
            case PERV_SQRT_INDEX:         // square root 
                fval1 = BIEVAL_evalFloat(argVec);
                if (fval1 < 0.0) EM_error(BI_ERROR_NEG_SQRT);
                return sqrt(fval1);
            case PERV_SIN_INDEX:         // sine 
                return sin(BIEVAL_evalFloat(argVec));
            case PERV_COS_INDEX:         // cosine
                return cos(BIEVAL_evalFloat(argVec));
            case PERV_ARCTAN_INDEX:      // arc tan 
                return atan(BIEVAL_evalFloat(argVec));
            case PERV_LOG_INDEX:         // natural log 
                fval1 = BIEVAL_evalFloat(argVec);
                if (fval1 <= 0.0) EM_error(BI_ERROR_NEG_SQRT);
                return log(fval1);
            case PERV_RABS_INDEX:        // real abs 
                fval1 = BIEVAL_evalFloat(argVec);
                return (fval1 <= 0.0 ? -fval1 : fval1);
            case PERV_ITOR_INDEX :       // coercion to real 
                return (float)BIEVAL_evalInt(argVec);
            default: EM_error(BI_ERROR_CONST_IND, cstInd);
            } //switch
        } else EM_error(BI_ERROR_ILLEGAL_ARG, mytmPtr);  //not atomic or app
    } //non atomic

    //Impossible to reach this point.
    return 0.0f;
}

/***********************************************************************/
/* auxiliary functions for string typed term operations                */
/***********************************************************************/
static DF_StrDataPtr BIEVAL_strConcat(MCSTR_Str str1, MCSTR_Str str2)
{
    int length = MCSTR_strLength(str1) + MCSTR_strLength(str2);
    int size   = MCSTR_numWords(length);
    MemPtr strDataHead = AM_hreg;
    MemPtr strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    MemPtr nhreg = strData + size;
    
    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead); 
    MCSTR_concat((MCSTR_Str)strData, str1, str2);
    
    AM_hreg = nhreg;
    return (DF_StrDataPtr)strDataHead;
}   

static DF_StrDataPtr BIEVAL_subString(MCSTR_Str str, int startPos, int endPos)
{
    int length = MCSTR_strLength(str);
    int size   = MCSTR_numWords(length);
    MemPtr strDataHead = AM_hreg;
    MemPtr strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    MemPtr nhreg;
      

    if (startPos < 0 || endPos > length || endPos < startPos) {
      EM_error(BI_ERROR_SUBSTRING);
    }

    length = endPos - startPos;
    
    nhreg = strData + size;
    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead);
    MCSTR_subString((MCSTR_Str)strData, str, startPos, length);
    AM_hreg = nhreg;
    return (DF_StrDataPtr)strDataHead;
}

static DF_StrDataPtr BIEVAL_chr(int i)
{
    int size = MCSTR_numWords(1);
    MemPtr strDataHead = AM_hreg;
    MemPtr strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    MemPtr nhreg       = strData + size;
    
    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead);
    MCSTR_chr((MCSTR_Str)strData, i);
    
    AM_hreg = nhreg;
    return (DF_StrDataPtr)strDataHead;
}

static DF_StrDataPtr BIEVAL_intToStr(int i)
{
    MemPtr strDataHead = AM_hreg;
    MemPtr strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    char   buf[128];
    int    length;
    int    size;
    MemPtr nhreg;
    
    sprintf(buf, "%d", i);
    length = strlen(buf);
    size   = MCSTR_numWords(length);
    nhreg = strData + size;
    
    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead);
    MCSTR_toString((MCSTR_Str)strData, buf, length);
    
    AM_hreg = nhreg;
    return (DF_StrDataPtr)strDataHead;
}

static DF_StrDataPtr BIEVAL_floatToStr(float f)
{
    MemPtr strDataHead = AM_hreg;
    MemPtr strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    char   buf[40];
    int    length;
    int    size;
    MemPtr nhreg;

    sprintf(buf, "%f", f);
    length = strlen(buf);
    size   = MCSTR_numWords(length);
    nhreg = strData + size;
    
    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead);
    MCSTR_toString((MCSTR_Str)strData, buf, length);
    
    AM_hreg = nhreg;
    return (DF_StrDataPtr)strDataHead;
}

/*************************************************************************
 *                                                                       *
 *  evaluation on expressions of string type                             *
 *                                                                       *
 *************************************************************************/
DF_StrDataPtr BIEVAL_evalStr(DF_TermPtr tmPtr)
{
    DF_TermPtr mytmPtr;
    HN_hnorm(tmPtr);
    mytmPtr = DF_termDeref(tmPtr);
    if (DF_isAtomic(mytmPtr)) {
        switch (DF_termTag(AM_head)) {
        case DF_TM_TAG_STR: return DF_strData(AM_head); 
        case DF_TM_TAG_VAR: EM_error(BI_ERROR_FLEX_HEAD, mytmPtr);
        default:            EM_error(BI_ERROR_ILLEGAL_ARG, mytmPtr);
        }
    } else { //non atomic term
      if (DF_isApp(mytmPtr)) {
	      DF_TermPtr argVec = AM_argVec;
	      int        cstInd;
      	
	      if (!AM_rigFlag) EM_error(BI_ERROR_FLEX_HEAD, mytmPtr); //non cst hd
                  
	      cstInd = DF_constTabIndex(AM_head);
	      switch(cstInd) {
	      case PERV_SCAT_INDEX:
	        return BIEVAL_strConcat(DF_strDataValue(BIEVAL_evalStr(argVec)),
				        DF_strDataValue(BIEVAL_evalStr(argVec+1)));
	      case PERV_SUBSTR_INDEX:
	        {
	          DF_StrDataPtr str  = BIEVAL_evalStr(argVec);
	          int           ival = BIEVAL_evalInt(argVec+1);
	          return BIEVAL_subString(DF_strDataValue(str), ival,
				          ival+BIEVAL_evalInt(argVec+2));
	        }
	      case PERV_ITOCHR_INDEX:
	        return BIEVAL_chr(BIEVAL_evalInt(argVec));
	      case PERV_ITOSTR_INDEX:
	        return BIEVAL_intToStr(BIEVAL_evalInt(argVec));
	      case PERV_RTOS_INDEX:
	        return BIEVAL_floatToStr(BIEVAL_evalFloat(argVec));
	      default:   EM_error(BI_ERROR_CONST_IND, cstInd);
	      } //switch
      } else EM_error(BI_ERROR_ILLEGAL_ARG, mytmPtr);//non-atomic and non-app
    } //non atomic 

    //Impossible to reach this point.
    return NULL;
}

    
    
    
    


