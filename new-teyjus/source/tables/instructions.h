/****************************************************************************/
/*                                                                          */
/*   File  instructions.h.  This header file defines the instruction        */
/*   categories according to argument types and op codes. It also defines   */
/*   a structure that is used to store information about each instruction.  */
/*   See the file instructions.c for information for each specific          */
/*   instruction.                                                           */
/*                                                                          */
/****************************************************************************/
#ifndef INSTRUCTIONS_H
#define INSTRUCTIONS_H


/***************************************************************************/
/*              OPERAND TYPES                                              */
/***************************************************************************/

/* possible types of instruction operands                                  */
typedef enum INSTR_OperandType
{
    INSTR_P,                    //(1 byte) padding
    INSTR_R,                    //argument register number
    INSTR_E,                    //environment variable number
    INSTR_N,                    //next clause number in impt/impl point
    INSTR_I1,                   //1 byte natural number
    INSTR_I2,                   //2 bytes natural number
    INSTR_CE,                   //closure environment variable number
    INSTR_C,                    //constant symbol table index
    INSTR_K,                    //kind symbol table index
    INSTR_MT,                   //module table address
    INSTR_IT,                   //impl table address
    INSTR_HT,                   //hash table address
    INSTR_BVT,                  //branch table for bound variable indexing
    INSTR_L,                    //code location
    INSTR_I,                    //integer immediate value
    INSTR_F,                    //floating point immediate value
    INSTR_S,                    //string pointer
    INSTR_X                     //operand list terminator
} INSTR_OperandType;


/***************************************************************************/
/*            INSTRUCTION CATEGORIES                                       */
/***************************************************************************/ 

/* The names of instruction categories no longer include padding bytes.    */
/* Thus we do not need to maintain two sets of names for different machine */
/* architectures.                                                          */
typedef enum INSTR_InstrCategory
{
    INSTR_CAT_X           = 0,
    INSTR_CAT_R_X         = 1,
    INSTR_CAT_E_X         = 2,
    INSTR_CAT_I1_X        = 3,
    // ...
    INSTR_CAT_R_C_I2_X    = 17,
    //ADDED categories
	INSTR_CAT_C_X         = 21,
	INSTR_CAT_L_X         = 22,
	INSTR_CAT_I1_C_X      = 23,
	//...
    INSTR_CAT_I1_L_X      = 24,
    // ...
    INSTR_CAT_I1_HT_X     = 26,
    // ...
    INSTR_CAT_N_L_L_X     = 33,
    INSTR_CAT_L_L_L_L_X   = 34,
    // ...
    INSTR_CAT_I1_N_X      = 45,
    INSTR_CAT_I2_BVT_X    = 46,
    // ...
    INSTR_CAT_I1_L_L_X    = 55,
    INSTR_NUM_INSTR_CATS  = 56   //number of instruction categories (temp)
} INSTR_InstrCategory;


/* Lengths of instructions in the different categories in words.            */
/* The assumption is that the op code occupies 1 byte.                      */

//those not depend on machine architecture
#define INSTR_CAT_X_LEN                     1
#define INSTR_CAT_R_X_LEN                   1
#define INSTR_CAT_E_X_LEN                   1
#define INSTR_CAT_I1_X_LEN                  1
#define INSTR_CAT_I1_L_X_LEN                2

//ADDED length
#define INSTR_CAT_L_X                       2

#define INSTR_CAT_I1_HT_X_LEN               2
#define INSTR_CAT_N_L_L_X_LEN               3
#define INSTR_CAT_L_L_L_L_X_LEN             5
#define INSTR_CAT_I1_N_X_LEN                1
#define INSTR_CAT_I2_BVT_X_LEN              2
#define INSTR_CAT_I1_L_L_X_LEN              3

// ...

//those depend on machine architecture
#ifdef MACH_64
#define INSTR_CAT_R_C_I2_LEN                1
//...to be filled in
#else  //MACH_64 (32-bit machine) 
#define INSTR_CAT_R_C_I2_LEN                2
//...to be filled in 
#endif //MACH_64


/* Distances between instruction operands in bytes.                         */
/* Used by the instruction interpreter (simulator/simulator.c)              */ 
#ifdef MACH_64
#define INSTR_LLen                          8

#else  //MACH_64 (32-bit machine) 
#define INSTR_LLen                          4

#endif //MACH_64 

/****************************************************************************/
/*               OPERAND TYPES TABLE                                        */
/****************************************************************************/

//the operand types array in a given entry
INSTR_OperandType* INSTR_operandTypes(INSTR_InstrCategory index);

/***************************************************************************/
/*              OPCODES OF INSTRUCTIONS                                    */
/***************************************************************************/
#define   unify_nil                           0
//...to be filled in

//ADDED Instructions
#define   call                                50
#define   call_name                           51
#define   execute                             52
#define   execute_name                        53
#define   fail                                54

/* Choice Instructions  */

#define   trust_me                            140
#define   trust_ext                           142
#define   retry_me_else                       143
#define   retry                               144
#define   trust                               145
#define   try_me_else                         146
#define   try                                 147
#define   try_else                            148
#define   retry_else                          149


/* Indexing Instructions */

#define   switch_on_constant                  150
#define   switch_on_bvar                      151
#define   switch_on_reg                       152
#define   switch_on_term                      153
//...to be filled in


#define   instr_end                           181
#define   INSTR_NUM_INSTRS                    182 

/***************************************************************************/
/*              INSTRUCTION INFORMATION TABLE                              */
/***************************************************************************/
INSTR_InstrCategory INSTR_instrType(int index);  //instr type in a given entry 
char*               INSTR_instrName(int index);  //instr name in a given entry
int                 INSTR_instrSize(int index);  //instr size in a given entry


#endif //INSTRUCTIONS_H
