/****************************************************************************/
/*                                                                          */
/*   File  instructions.c. This file defines the operand types table and    */
/*   the instruction information table.                                     */
/*                                                                          */
/****************************************************************************/

#ifndef INSTRUCTIONS_C
#define INSTRUCTIONS_C

#include "instructions.h"


/****************************************************************************/
/*    INSTRUCTION INFORMATION TABLE                                         */
/****************************************************************************/
typedef struct                        //entry of the instruction info table
{
    char* name;
    INSTR_InstrCategory type;
    int   size;
} INSTR_InstrInfoTab_;

typedef INSTR_InstrInfoTab_ INSTR_InstrInfoTab[INSTR_NUM_INSTRS]; 


INSTR_InstrInfoTab INSTR_instrInfoTable ={
{ "unify_nil",            INSTR_CAT_X,          INSTR_CAT_X_LEN},
//... to be filled in

//To be moved
[call]=
{ "call",                 INSTR_CAT_I1_L_X,     INSTR_CAT_I1_L_X_LEN},
[call_name]=
{ "call_name",            INSTR_CAT_I1_C_X,     INSTR_CAT_I1_L_X_LEN},

[execute]=
{ "call",                 INSTR_CAT_L_X,        INSTR_CAT_L_X_LEN},
[execute_name]=
{ "call_name",            INSTR_CAT_C_X,        INSTR_CAT_L_X_LEN},

[fail]=
{ "fail",                 INSTR_CAT_X,          INSTR_CAT_X_LEN},

[140] = //C99 feature (not needed when the table is fully filled in)
{ "trust_me",             INSTR_CAT_I1_X,       INSTR_CAT_I1_L_X_LEN},
[142] =
{ "trust_ext",            INSTR_CAT_I1_N_X,     INSTR_CAT_I1_N_X_LEN},
[143] =
{ "retry_me_else",        INSTR_CAT_I1_L_X,     INSTR_CAT_I1_L_X_LEN},
[144] =
{ "retry",                INSTR_CAT_I1_L_X,     INSTR_CAT_I1_L_X_LEN},
[145] =
{ "trust",                INSTR_CAT_I1_L_X,     INSTR_CAT_I1_L_L_X_LEN},
[146] =
{ "try_me_else",          INSTR_CAT_I1_L_X,     INSTR_CAT_I1_L_X_LEN},
[147] =
{ "try",                  INSTR_CAT_I1_L_X,     INSTR_CAT_I1_L_X_LEN},
[148] =
{ "try_else",             INSTR_CAT_I1_L_L_X,   INSTR_CAT_I1_L_L_X_LEN},
[149] =
{ "retry_else",           INSTR_CAT_I1_L_L_X,   INSTR_CAT_I1_L_L_X_LEN},

[150] =
{ "switch_on_constant",   INSTR_CAT_I1_HT_X,    INSTR_CAT_I1_HT_X_LEN},
[151] =
{ "switch_on_bvar",       INSTR_CAT_I2_BVT_X,   INSTR_CAT_I2_BVT_X_LEN},
[152] =
{ "switch_on_reg",        INSTR_CAT_N_L_L_X,    INSTR_CAT_N_L_L_X_LEN},
[153] =
{ "switch_on_term",       INSTR_CAT_L_L_L_L_X,  INSTR_CAT_L_L_L_L_X_LEN},
  
[181] =
{ "instr_end",            INSTR_CAT_X,          INSTR_CAT_X_LEN}
};

/* Accessing functions */
INSTR_InstrCategory INSTR_instrType(int index)
{
    return (INSTR_instrInfoTable[index]).type;
}

char* INSTR_instrName(int index)
{
    return (INSTR_instrInfoTable[index]).name;
}

int   INSTR_instrSize(int index)
{
    return (INSTR_instrInfoTable[index]).size;
}



/****************************************************************************/
/*    OPERAND TYPES TABLE                                                   */
/****************************************************************************/
/* Max number of operand that could be taken by instructions including the  */
/* padding bytes and one to terminate the list. (machine dependent)         */
#ifdef MACH_64

#define INSTR_MAX_OPERAND     12  //temp

#else  //MACH_64 (32-bit machine)

#define INSTR_MAX_OPERAND     8  

#endif //MACH_64


/* this array is indexed by instruction category.  For each category,
   INSTR_operandTypeTab contains a string of values indicating the type
   of the operand at that position, terminated by INSTR_X.  This
   information is useful when parsing instruction streams. */
typedef INSTR_OperandType 
        INSTR_OperandTypeTab[INSTR_NUM_INSTR_CATS][INSTR_MAX_OPERAND];

extern INSTR_OperandTypeTab INSTR_operandTypeTable;


#ifdef MACH_64

INSTR_OperandTypeTab INSTR_operandTypeTable ={
    {INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_X,
     INSTR_X, INSTR_X, INSTR_X, INSTR_X}, //INSTR_CAT_X
    {INSTR_R, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_X,
     INSTR_X, INSTR_X, INSTR_X, INSTR_X}, //INSTR_CAT_R_X
    {INSTR_E, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_X,
     INSTR_X, INSTR_X, INSTR_X, INSTR_X}, //INSTR_CAT_E_X
    {INSTR_I1, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_X,
     INSTR_X, INSTR_X, INSTR_X, INSTR_X}, //INSTR_CAT_I1_X

    [17] = //C99 feature (not needed when the table is fully filled in)
    {INSTR_R, INSTR_C, INSTR_I2, INSTR_P, INSTR_P, INSTR_X, INSTR_X, INSTR_X,
     INSTR_X, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_R_C_I2_X

    //ADDED
    [21] = 
    {INSTR_P, INSTR_C, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X,
    INSTR_X, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_C_X
    [22] = 
    {INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_L,
    INSTR_X, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_L_X
    [23] = 
    {INSTR_I1, INSTR_C, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X,
    INSTR_X, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_I1_C_X

    [24] = 
    {INSTR_I1, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_L,
     INSTR_X, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_I1_L_X

    [26] = 
    {INSTR_I1, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_HT,
     INSTR_X, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_I1_HT_X

    [33] = 
    {INSTR_N, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_L,
     INSTR_L, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_N_L_L_X
    [34] = 
    {INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_L,
     INSTR_L, INSTR_L, INSTR_L, INSTR_X},  //INSTR_CAT_L_L_L_L_X

    [45] = 
    {INSTR_I1, INSTR_N, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_X,
     INSTR_X, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_I1_N_X
    [46] = 
    {INSTR_P, INSTR_I2, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_BVT, INSTR_X,
     INSTR_X, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_I2_BVT_X

    [55] = 
    {INSTR_I1, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_P, INSTR_L,
     INSTR_L, INSTR_X, INSTR_X, INSTR_X},  //INSTR_CAT_I1_L_L_X
};


#else  //MACH_64 (32-bit machines)
INSTR_OperandTypeTab INSTR_operandTypeTable ={
    //INSTR_CAT_X
    {INSTR_P, INSTR_P, INSTR_P, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X},
    //INSTR_CAT_R_X
    {INSTR_R, INSTR_P, INSTR_P, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X},
    //INSTR_CAT_E_X
    {INSTR_E, INSTR_P, INSTR_P, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X},
    //INSTR_CAT_I1_X
    {INSTR_I1, INSTR_P, INSTR_P, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X},

    //INSTR_CAT_R_C_I2   
    [17] = //C99 feature (not needed when the table is fully filled in)
    {INSTR_R, INSTR_C, INSTR_I2, INSTR_P, INSTR_P, INSTR_X, INSTR_X, INSTR_X},

    //ADDED
	[21] = //INSTR_CAT_C_X
	{INSTR_C, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X},
	[22] = //INSTR_CAT_L_X
	{INSTR_P, INSTR_P, INSTR_P, INSTR_L, INSTR_X, INSTR_X, INSTR_X, INSTR_X},
	[23] = //INSTR_CAT_I1_C_X
	{INSTR_I1, INSTR_C, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X},

    //INSTR_CAT_I1_L_X
    [24] =
    {INSTR_I1, INSTR_P, INSTR_P, INSTR_L, INSTR_X, INSTR_X, INSTR_X, INSTR_X},

    //INSTR_CAT_I1_HT_X
    [26] = 
    {INSTR_I1, INSTR_P, INSTR_P, INSTR_HT, INSTR_X, INSTR_X, INSTR_X, INSTR_X},

    //INSTR_CAT_N_L_L_X
    [33] = 
    {INSTR_N, INSTR_P, INSTR_P, INSTR_L, INSTR_L, INSTR_X, INSTR_X, INSTR_X},
    //INSTR_CAT_L_L_L_L_X
    [34] = 
    {INSTR_P, INSTR_P, INSTR_P, INSTR_L, INSTR_L, INSTR_L, INSTR_L, INSTR_X},

    //INSTR_CAT_I1_N_X
    [45] = 
    {INSTR_I1, INSTR_N, INSTR_P, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X},
    //INSTR_CAT_I2_BVT_X
    [46] = 
    {INSTR_P, INSTR_I2, INSTR_BVT, INSTR_X, INSTR_X, INSTR_X, INSTR_X, INSTR_X},

    //INSTR_CAT_I1_L_L_X
    [55] = 
    {INSTR_I1, INSTR_P, INSTR_P, INSTR_L, INSTR_L, INSTR_X, INSTR_X, INSTR_X}
};

#endif //MACH_64 (32-bit machines)

INSTR_OperandType* INSTR_operandTypes(INSTR_InstrCategory index)
{
    return INSTR_operandTypeTable[index];
}


#endif //INSTRUCTIONS_C
     
