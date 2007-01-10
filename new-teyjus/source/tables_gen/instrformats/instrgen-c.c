/******************************************************************************/
/* File instrgen-c.h. This files contains function declarations for generating*/
/* files instructions.h and instructions.c                                    */
/******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "instrgen-c.h"


#define INDENT1        "    "
#define INDENT1_LEN    4
#define PREFIX         "INSTR_"
#define PREFIX_LEN     6
#define CATPREFIX      "INSTR_CAT_"
#define CATPREFIX_LEN  10
#define DEF            "#define "
#define DEF_LEN        8

//covert an non-negtive integer to string
static char* itoa(int num)
{
    char *str;
    char digit[2];
    digit[1] = '\0';
    str = strdup("");
    do {
        char *temp = str;
        digit[0] = num % 10 + '0';
        str = (char*)malloc(sizeof(char) * (strlen(str) + 2));
        strcpy(str, digit);
        strcat(str, temp);
        free(temp);
    } while ((num /= 10) > 0);
    return str;
}

/*************************************************************************/
/* instructions.h                                                        */
/*************************************************************************/ 
#define COMMENTS_BEG_H \
"/****************************************************************************/\n/* File instructions.h.                                                     */ \n/* This file defines instruction operand types, instruction categories and  */ \n/* instruction opcode.                                                      */ \n/****************************************************************************/ \n"

#define COMPDEF_BEG_H "#ifndef INSTRUCTIONS_H\n#define INSTRUCTIONS_H\n"
#define COMPDEF_END_H "#endif //INSTRUCTIONS_H\n"

#define INCLUDE_H \
"#include \"../simulator/mctypes.h\"      //to be changed                      \n#include \"../simulator/dataformats.h\"   //to be changed                      \n"

/* OPERAND TYPES */
#define OPTYPES_COMMENTS_H \
"/****************************************************************************/\n/*              OPERAND TYPES                                               */ \n/****************************************************************************/ \n\n"
#define OPTYPES_COMMENTS_H_LEN 300

#define OPERAND_TYPE_BEG \
"/* possible types of instruction operands                                 */  \ntypedef enum INSTR_OperandType                                                 \n{\n"
#define OPERAND_TYPE_BEG_LEN   200

#define OPERAND_TYPE_END "} INSTR_OperandType;\n\n"
#define OPERAND_TYPE_END_LEN   30

static char* opTypes = NULL;
static char* opTypeMaps = NULL ;

void genOpTypes(char *name, char* typeName, char* types, char* comments, 
                int last)
{
    char* myOpTypes = opTypes;
    char* myOpTypeMaps = opTypeMaps;
    int   length;
    int   commentLen = comments ?  strlen(comments) : 0;
    length = (opTypes ? strlen(opTypes) : 0) + INDENT1_LEN + PREFIX_LEN + 
             strlen(name) + (comments ?  strlen(comments) : 0) + 30 ;
    opTypes = (char*)malloc(sizeof(char)*(length + 1));
    if (myOpTypes) { strcpy(opTypes, myOpTypes); strcat(opTypes, INDENT1); }
    else strcpy(opTypes, INDENT1);
    if (comments) {
        strcat(opTypes, "//");
        strcat(opTypes, comments);
        strcat(opTypes, "\n");
        strcat(opTypes, INDENT1);
    }
    strcat(opTypes, PREFIX);
    strcat(opTypes, name);
    if (last) strcat(opTypes, "\n");
    else strcat(opTypes, ",\n");
    if (myOpTypes) free(myOpTypes);
    
    if (typeName) {
        length = (opTypeMaps ? strlen(opTypeMaps) : 0) + PREFIX_LEN + 
            strlen(types) + strlen(typeName) + 30;
        opTypeMaps = (char*)malloc(sizeof(char)*(length + 1));
        if (myOpTypeMaps) { 
            strcpy(opTypeMaps, myOpTypeMaps); 
            strcat(opTypeMaps, "typedef ");
        } else strcpy(opTypeMaps, "typedef ");
        strcat(opTypeMaps, types);
        strcat(opTypeMaps, "  ");
        strcat(opTypeMaps, PREFIX);
        strcat(opTypeMaps, typeName);
        strcat(opTypeMaps, ";\n");
        if (myOpTypeMaps) free(myOpTypeMaps);
    }
}

#define OPTYPEMAP_COMMENT \
"/**************************************************************************/  \n/*                  Types for instruction operants                        */   \n/**************************************************************************/   \n\n"
#define OPTYPEMAP_COMMENT_LEN 300

static char *opcodeType = NULL;

void genOpCodeType(char* optype)
{
    int length = PREFIX_LEN + strlen(optype) + 50;
    opcodeType = (char*)malloc(sizeof(char)*(length + 1));
    strcpy(opcodeType, "typedef ");
    strcat(opcodeType, optype);
    strcat(opcodeType, "  ");
    strcat(opcodeType, PREFIX);
    strcat(opcodeType, "OpCode;\n");
}


static char *opsH = NULL;

void genOpsH() //assume neither opTypes nor opTypeMaps is empty
{
    int length = OPTYPES_COMMENTS_H_LEN + OPERAND_TYPE_BEG_LEN +  
        OPTYPEMAP_COMMENT_LEN + OPERAND_TYPE_END_LEN + strlen(opTypes) + 
        strlen(opTypeMaps) + strlen(opcodeType) + 50;
    opsH = (char*)malloc(sizeof(char)*(length+1));
    
    strcpy(opsH, OPTYPES_COMMENTS_H);
    strcat(opsH, OPERAND_TYPE_BEG);
    strcat(opsH, opTypes);
    strcat(opsH, OPERAND_TYPE_END);
    strcat(opsH, OPTYPEMAP_COMMENT);
    strcat(opsH, opcodeType);
    strcat(opsH, opTypeMaps);

    free(opTypes);
    free(opcodeType);
    free(opTypeMaps);
}

/* INSTRUCTION CATEGORIES */
#define INSTRCAT_COMMENTS_H \
"/***************************************************************************/ \n/*            INSTRUCTION CATEGORIES                                       */  \n/***************************************************************************/  \n"
#define INSTRCAT_COMMENTS_H_LEN 300

#define INSTRCAT_TYPE_BEG \
" /* The names of instruction categories no longer include padding bytes.    */\n/* Thus we do not need to maintain two sets of names for different machine */  \n/* architectures.                                                          */  \ntypedef enum INSTR_InstrCategory                                               \n{\n"
#define INSTRCAT_TYPE_BEG_LEN 350
#define INSTRCAT_TYPE_END "} INSTR_InstrCategory;\n\n"
#define INSTRCAT_TYPE_END_LEN 50


static char *instrcat_type = NULL;
static char *instrLen = NULL;
static char *oneInstrLen = NULL;
static int  catNum = 0;

void genOneInstrCatH(char* name, int last)
{   
    char *myInstrCat = instrcat_type, *myInstrLen = instrLen;
    int   length = (myInstrCat ? strlen(myInstrCat) : 0) + strlen(name) +
        CATPREFIX_LEN + INDENT1_LEN + 10;
    instrcat_type = (char*)malloc(sizeof(char)*(length+1));
    if (myInstrCat) {
        strcpy(instrcat_type, myInstrCat);
        strcat(instrcat_type, INDENT1);
    } else strcpy(instrcat_type, INDENT1);
    strcat(instrcat_type, CATPREFIX);
    strcat(instrcat_type, name);
    strcat(instrcat_type, " = ");
    strcat(instrcat_type, itoa(catNum));
    if (last) strcat(instrcat_type, "\n");
    else strcat(instrcat_type, ",\n");
    catNum++;

    if (myInstrCat) free(myInstrCat);

    //assume oneInstrLen cannot be empty
    length = (myInstrLen ? strlen(myInstrLen) : 0) + strlen(name) + 
        CATPREFIX_LEN + 10 + strlen(oneInstrLen);
    instrLen = (char*)malloc(sizeof(char)*(length + 1));
    
    if (myInstrLen) {
        strcpy(instrLen, myInstrLen);
        strcat(instrLen, "//");
    } else strcpy(instrLen, "//");
    strcat(instrLen, CATPREFIX);
    strcat(instrLen, name);
    strcat(instrLen, "\n");
    strcat(instrLen, oneInstrLen);
    
    free(oneInstrLen);
    oneInstrLen = NULL;
    if (myInstrLen) free(myInstrLen);
}

#define INSTRLEN_COMMENTS \
"/**************************************************************************/  \n/* Macros defines instruction lengths and distances between op code and   */   \n/* operands.                                                              */   \n/* The assumption is that the op code occupies 1 byte.                    */   \n/**************************************************************************/   \n\n"
#define INSTRLEN_COMMENTS_LEN 450

void genInstrLength(char* name, char* len)
{
    char *myInstrLen = oneInstrLen;    
    int length = (myInstrLen ? strlen(myInstrLen) : 0) + DEF_LEN + PREFIX_LEN 
        + strlen(name) + strlen(len) + 10;
    oneInstrLen = (char*)malloc(sizeof(char)*(length + 1));
    if (myInstrLen) {
        strcpy(oneInstrLen, myInstrLen);
        strcat(oneInstrLen, DEF);
    } else strcpy(oneInstrLen, DEF);
    strcat(oneInstrLen, PREFIX);
    strcat(oneInstrLen, name);
    strcat(oneInstrLen, "    ");
    strcat(oneInstrLen, len);
    strcat(oneInstrLen, "\n");
    
    free(myInstrLen);
}

#define OPTYPE_TAB_H \
"/****************************************************************************/\n/*               OPERAND TYPES TABLE                                        */ \n/****************************************************************************/ \n                                                                               \n//the operand types array in a given entry                                     \nINSTR_OperandType* INSTR_operandTypes(INSTR_InstrCategory index);              \n"
#define OPTYPE_TAB_H_LEN 500


static char *instrCatH = NULL;

void genInstrCatH(char* callI1Len)
{
    int length = strlen(instrcat_type) + strlen(instrLen) + 
        INSTRCAT_TYPE_BEG_LEN + INSTRCAT_TYPE_END_LEN + INSTRCAT_COMMENTS_H_LEN 
        + INSTRLEN_COMMENTS_LEN + OPTYPE_TAB_H_LEN + 160;
    instrCatH = (char*)malloc(sizeof(char)*(length+1));
    
    strcpy(instrCatH, INSTRCAT_COMMENTS_H);
    strcat(instrCatH, INSTRCAT_TYPE_BEG);
    strcat(instrCatH, instrcat_type);
    strcat(instrCatH, INSTRCAT_TYPE_END);
    strcat(instrCatH, DEF);
    strcat(instrCatH, "INSTR_NUM_INSTR_CATS  ");
    strcat(instrCatH, itoa(catNum));
    strcat(instrCatH, "\n\n");
    strcat(instrCatH, DEF);
    strcat(instrCatH, "INSTR_CALL_I1_LEN  ");
    strcat(instrCatH, callI1Len);
    strcat(instrCatH, "\n\n");
    strcat(instrCatH, INSTRLEN_COMMENTS);
    strcat(instrCatH, instrLen);
    strcat(instrCatH, "\n");
    strcat(instrCatH, OPTYPE_TAB_H);
    

    free(instrcat_type);
    free(instrLen);
}

#define INSTR_COMMENTS_H \
"/***************************************************************************/ \n/*              OPCODES OF INSTRUCTIONS                                    */  \n/***************************************************************************/  \n"
#define INSTR_COMMENTS_H_LEN  250

static char* instrH = NULL;

void genOneInstrH(char* comments, char* opCode, char* instrName)
{
    char* myInstrH = instrH;
    int length = (myInstrH ? strlen(myInstrH) : 0) + strlen(instrName) +
        strlen(opCode) + DEF_LEN + CATPREFIX_LEN + 
        (comments ? strlen(comments) : 0) + 10;
    instrH = (char*)malloc(sizeof(char)*(length + 1));
    if (myInstrH) {
        strcpy(instrH, myInstrH);
        if (comments) {
            strcat(instrH, "//");
            strcat(instrH, comments);
            strcat(instrH, "\n");
            strcat(instrH, DEF);
        } else strcat(instrH, DEF);
    } else {
        if (comments) {
            strcpy(instrH, "//");
            strcat(instrH, comments);
            strcat(instrH, "\n");
            strcat(instrH, DEF);
        } else strcpy(instrH, DEF);
    }
    strcat(instrH, instrName);
    strcat(instrH, "   ");
    strcat(instrH, opCode);
    strcat(instrH, "\n");
    
    
    if (myInstrH) free(myInstrH);
}

#define INSTRTAB_H \
"/***************************************************************************/ \n/*              INSTRUCTION INFORMATION TABLE                              */ \n/***************************************************************************/  \nINSTR_InstrCategory INSTR_instrType(int index);  //instr type in a given entry \nchar*               INSTR_instrName(int index);  //instr name in a given entry \nint                 INSTR_instrSize(int index);  //instr size in a given entry \n"
#define INSTRTAB_H_LEN 500



char* instrOpc = NULL;

void genInstrH(char* numInstr)
{
    int length = INSTR_COMMENTS_H_LEN + strlen(instrH) + DEF_LEN + 
        strlen(numInstr) + INSTRTAB_H_LEN + 20;
    
    instrOpc = (char*)malloc(sizeof(char)*(length + 1));
    strcpy(instrOpc, INSTR_COMMENTS_H);
    strcat(instrOpc, instrH);
    strcat(instrOpc, "\n\n");
    strcat(instrOpc, DEF);
    strcat(instrOpc, "INSTR_NUM_INSTRS");
    strcat(instrOpc, "   ");
    strcat(instrOpc, numInstr);
    strcat(instrOpc, "\n\n");
    strcat(instrOpc, INSTRTAB_H);
    
    free(instrH);
}


/* dump instructions.h" */
void spitCInstructionsH()
{
    FILE* outFile;
    outFile = fopen("../../tables/instructions.h", "w");
    if (outFile) {
        fprintf(outFile, "%s\n%s\n%s\n", COMMENTS_BEG_H, COMPDEF_BEG_H, 
                INCLUDE_H);
        fprintf(outFile, "%s\n", opsH);
        fprintf(outFile, "%s\n", instrCatH);
        fprintf(outFile, "%s\n", instrOpc);
        fprintf(outFile, "%s\n", COMPDEF_END_H);
        fclose(outFile);

        free(opsH);
        free(instrCatH);
        free(instrOpc);
    } else printf("cannot open\n");
}


/*************************************************************************/
/* instructions.c                                                        */
/*************************************************************************/ 
#define COMMENTS_BEG_C \
"/****************************************************************************/\n/*                                                                          */ \n/*   File  instructions.c. This file defines the operand types table and    */ \n/*   the instruction information table.                                     */ \n/*                                                                          */ \n/****************************************************************************/ \n\n"
#define INCLUDE_C "#include \"instructions.h\"\n"

/*OPERAND TYPE TABLE */
#define OPTYPE_TAB_COMMENTS \
"/****************************************************************************/\n/*    OPERAND TYPES TABLE                                                   */ \n/****************************************************************************/ \n\n"
#define OPTYPE_TAB_COMMENTS_LEN 250

#define MAX_OP_COMMENTS \
"/* Max number of operand that could be taken by instructions including the  */\n/* padding bytes and one to terminate the list. (machine dependent)         */ \n"
#define MAX_OP_COMMENTS_LEN     200


#define OPTYPE_TAB_TYPE \
"/* this array is indexed by instruction category.  For each category,         \n   INSTR_operandTypeTab contains a string of values indicating the type        \n   of the operand at that position, terminated by INSTR_X.  This               \n   information is useful when parsing instruction streams. */                  \ntypedef INSTR_OperandType                                                      \n        INSTR_OperandTypeTab[INSTR_NUM_INSTR_CATS][INSTR_MAX_OPERAND];         \n\n"

#define OPTYPE_TAB_TYPE_LEN     500

#define OPTYPE_TAB_BEG "INSTR_OperandTypeTab INSTR_operandTypeTable ={\n" 
#define OPTYPE_TAB_BEG_LEN      80
#define OPTYPE_TAB_END "};\n\n"
#define OPTYPE_TAB_END_LEN      10

static char* optypeTabEntry = NULL;

void genInstrFormat(char* opType, int last)
{
    char* mytabEntry = optypeTabEntry;
    int length = (mytabEntry ? strlen(mytabEntry) : 0) + PREFIX_LEN + 
        strlen(opType) + 5;
    optypeTabEntry = (char*)malloc(sizeof(char)*(length + 1));
    
    if (mytabEntry) {
        strcpy(optypeTabEntry, mytabEntry);
        strcat(optypeTabEntry, PREFIX);
    } else strcpy(optypeTabEntry, PREFIX);
    strcat(optypeTabEntry, opType);
    if (!last) strcat(optypeTabEntry, ", ");
    
    if (mytabEntry) free(mytabEntry);
}

static char* optypeTab = NULL;

//assume optypeEntry is not empty
void genOneInstrCatC(char* name, int last)
{
    char* myoptypeTab = optypeTab;
    int length = (myoptypeTab ? strlen(myoptypeTab) : 0) + INDENT1_LEN*2 + 
        strlen(optypeTabEntry) + strlen(name) + 10 + CATPREFIX_LEN;
    
    optypeTab = (char*)malloc(sizeof(char)*(length + 1));
    
    if (myoptypeTab) {
        strcpy(optypeTab, myoptypeTab);
        strcat(optypeTab, INDENT1);
    } else strcpy(optypeTab, INDENT1);
    strcat(optypeTab, "//");
    strcat(optypeTab, CATPREFIX);
    strcat(optypeTab, name);
    strcat(optypeTab, "\n");
    strcat(optypeTab, INDENT1);
    strcat(optypeTab, "{");
    strcat(optypeTab, optypeTabEntry);
    if (last) strcat(optypeTab, "}\n");
    else strcat(optypeTab, "},\n");
    
    free(optypeTabEntry);
    optypeTabEntry = NULL;
    if (myoptypeTab) free(myoptypeTab);
}

#define OPTYPE_FUNC \
"INSTR_OperandType* INSTR_operandTypes(INSTR_InstrCategory index)              \n{                                                                              \n   return INSTR_operandTypeTable[index];                                       \n}\n"
#define OPTYPE_FUNC_LEN 250

static char* opTypeC = NULL;

void genInstrCatC(char* max_op){
    int length = OPTYPE_TAB_COMMENTS_LEN + MAX_OP_COMMENTS_LEN + 
        OPTYPE_TAB_TYPE_LEN +  OPTYPE_TAB_BEG_LEN + OPTYPE_TAB_END_LEN +
        strlen(optypeTab) + OPTYPE_FUNC_LEN + strlen(max_op) + 100;
    opTypeC = (char*)malloc(sizeof(char)*(length+1));
    strcpy(opTypeC, OPTYPE_TAB_COMMENTS);
    strcat(opTypeC, MAX_OP_COMMENTS);
    strcat(opTypeC, "#define INSTR_MAX_OPERAND     ");
    strcat(opTypeC, max_op);
    strcat(opTypeC, "\n\n");
    strcat(opTypeC, OPTYPE_TAB_TYPE);
    strcat(opTypeC, OPTYPE_TAB_BEG);
    strcat(opTypeC, optypeTab);
    strcat(opTypeC, OPTYPE_TAB_END);
    strcat(opTypeC, OPTYPE_FUNC);
}

//dynamic string array type
typedef struct StringArray 
{
  char **array;
  int  length;
} StringArray;


static StringArray instrTab;

void initInstrC(int numInstrs)
{
    instrTab.length = numInstrs;  
    instrTab.array = (char**)malloc(sizeof(char*)*numInstrs);
}

void genOneInstrC(int opcode, char* name, char* cat, char* len,  int last) 
{   
    int length = strlen(name) + strlen(cat) + strlen(len) + PREFIX_LEN 
        + CATPREFIX_LEN + 20 + INDENT1_LEN ;
    char* myText = (char*)malloc(sizeof(char)*(length + 1));
    
    strcpy(myText, INDENT1);
    strcat(myText, "{\"");
    strcat(myText, name);
    strcat(myText, "\",  ");
    strcat(myText, CATPREFIX);
    strcat(myText, cat);
    strcat(myText, ",   ");
    strcat(myText, PREFIX);
    strcat(myText, len);
    if (last) strcat(myText, "}\n");
    else strcat(myText, "},\n");
    
    instrTab.array[opcode] = myText;
}

#define INSTR_TAB_C_COMMENTS \
"/****************************************************************************/\n/*    INSTRUCTION INFORMATION TABLE                                         */ \n/****************************************************************************/ \n"
#define INSTR_TAB_C_COMMENTS_LEN 250

#define INSTR_TAB_TYPE \
"typedef struct                        //entry of the instruction info table   \n{                                                                              \n    char* name;                                                                \n    INSTR_InstrCategory type;                                                  \n    int   size;                                                                \n} INSTR_InstrInfoTab_;                                                       \n\ntypedef INSTR_InstrInfoTab_ INSTR_InstrInfoTab[INSTR_NUM_INSTRS];              \n\n"
#define INSTR_TAB_TYPE_LEN  600

#define INSTR_TAB_BEG "INSTR_InstrInfoTab INSTR_instrInfoTable ={\n"
#define INSTR_TAB_BEG_LEN   80

#define INSTR_TAB_END "};\n\n"
#define INSTR_TAB_END_LEN   10

#define INSTR_TAB_FUNC_C \
"/* Accessing functions */                                                     \nINSTR_InstrCategory INSTR_instrType(int index)                                 \n{                                                                              \n    return (INSTR_instrInfoTable[index]).type;                                 \n}                                                                            \n\nchar* INSTR_instrName(int index)                                               \n{                                                                              \n    return (INSTR_instrInfoTable[index]).name;                                 \n}                                                                            \n\nint   INSTR_instrSize(int index)                                               \n{                                                                              \n    return (INSTR_instrInfoTable[index]).size;                                 \n}\n\n"
#define INSTR_TAB_FUNC_C_LEN 1000

static char* instrC = NULL;

void genInstrC()
{
    int i, length;
    char *myText = NULL, *myText2;
    for (i = 0; i < instrTab.length; i++) {
        if (instrTab.array[i]) {
            length = (myText ? strlen(myText) : 0) + strlen(instrTab.array[i]);
            myText2 = (char*)malloc(sizeof(char)*(length + 100));
            if (myText) {
                strcpy(myText2, myText);
                strcat(myText2, instrTab.array[i]);
            } else strcpy(myText2, instrTab.array[i]);
            free(instrTab.array[i]);
            free(myText);
            myText = myText2;
        }
    }
    free(instrTab.array);
    length = INSTR_TAB_C_COMMENTS_LEN + INSTR_TAB_TYPE_LEN + 
        INSTR_TAB_BEG_LEN + INSTR_TAB_END_LEN + INSTR_TAB_FUNC_C_LEN +
        strlen(myText);
    instrC = (char*)malloc(sizeof(char) * (length + 1));
    strcpy(instrC, INSTR_TAB_C_COMMENTS);
    strcat(instrC, INSTR_TAB_TYPE);
    strcat(instrC, INSTR_TAB_BEG);
    strcat(instrC, myText);
    strcat(instrC, INSTR_TAB_END);
    strcat(instrC, INSTR_TAB_FUNC_C);
    free(myText);
}


/* dump instructions.c" */
void spitCInstructionsC()
{
    FILE* outFile;
    outFile = fopen("../../tables/instructions.c", "w");
    if (outFile) {
        fprintf(outFile, "%s\n%s\n", COMMENTS_BEG_C, INCLUDE_C);
        fprintf(outFile, "%s\n", opTypeC);
        fprintf(outFile, "%s\n", instrC);
        fclose(outFile);
        free(opTypeC);
        free(instrC);
    } else printf("cannot open\n");
}

/* simdispatch.c    */
#define SIMPREFIX        "SINSTR_"
#define SIMPREFIX_LEN    7

#define SIMDIS_COMMENTS \
"/***************************************************************************/ \n/*                                                                         */  \n/*  File simdispatch.c. The instruction dispatch table used by the         */  \n/*  simulator is defined here as an array of function pointers, each of    */  \n/*  which refers to a function realizing a corresponding instruction.      */  \n/*  These functions are defined in the file ./siminstr.c.                  */  \n/***************************************************************************/  \n\n"
#define SIMDIS_COMMENTS_LEN  600

#define SIMDIS_INCLUDE \
"#include \"../tables/instructions.h\" //to be modified                        \n#include \"siminstr.h\"                                                        \n#include \"simdispatch.h\"\n\n"
#define SIMDIS_INCLUDE_LEN 250

#define SIMDIS_TAB_BEG \
"SDP_InstrFunctionPtr SDP_dispatchTable[INSTR_NUM_INSTRS] = {\n"
#define SIMDIS_TAB_BEG_LEN 80

#define SIMDIS_TAB_END "};\n"
#define SIMDIS_TAB_END_LEN 10

static StringArray dispatchTab;

void initSimDispatch(int size) 
{
    dispatchTab.length = size;
    dispatchTab.array = (char**)malloc(sizeof(char*)*size);
}

void genOneSimDispatch(int ind, char* instr, int last)
{
    int length = strlen(instr) + SIMPREFIX_LEN + INDENT1_LEN + 10;
    char* myText = (char*)malloc(sizeof(char)*(length + 1));
    
    strcpy(myText, INDENT1);
    strcat(myText, SIMPREFIX);
    strcat(myText, instr);
    if (last) strcat(myText, "\n");
    else strcat(myText, ",\n");
    
    dispatchTab.array[ind] = myText;
}

static char* dispatch = NULL;

void genSimDispatch()
{
    int i, length;
    char *myText = NULL, *myText2;
    
    for(i = 0; i < dispatchTab.length; i++) {
        if (dispatchTab.array[i]){
            length = (myText ? strlen(myText) : 0)+strlen(dispatchTab.array[i]);
            myText2 = (char*)malloc(sizeof(char)*(length + 1));
            if (myText){
                strcpy(myText2, myText);
                strcat(myText2, dispatchTab.array[i]);
            } else strcpy(myText2, dispatchTab.array[i]);
            free(dispatchTab.array[i]);
            free(myText);
            myText = myText2;
        }
    }
    free(dispatchTab.array);
    length = SIMDIS_COMMENTS_LEN + SIMDIS_INCLUDE_LEN + SIMDIS_TAB_BEG_LEN 
        + SIMDIS_TAB_BEG_LEN + SIMDIS_TAB_END_LEN + strlen(myText);
    dispatch = (char*)malloc(sizeof(char) * (length+ 1));
    strcpy(dispatch, SIMDIS_COMMENTS);
    strcat(dispatch, SIMDIS_INCLUDE);
    strcat(dispatch, SIMDIS_TAB_BEG);
    strcat(dispatch, myText);
    strcat(dispatch, SIMDIS_TAB_END);

    free(myText);
}

void spitSimDispatch()
{
    FILE* outFile;
    outFile = fopen("../../simulator/simdispatch.c", "w");
    if (outFile) {
        fprintf(outFile, "%s\n", dispatch);
        free(dispatch);
        fclose(outFile);
    } else printf("cannot open\n");
}




