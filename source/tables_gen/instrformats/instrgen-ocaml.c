/*************************************************************************/
/* functions for generating ocaml instr.mli and instr.ml                 */
/*************************************************************************/
#include "util.h"
#include <string.h>
#include <stdio.h>
#include <math.h>

static char* addLine(char* str, char* addOn)
{
    int length = (str ? strlen(str) : 0) + strlen(addOn) + 2;
    char* newStr = UTIL_mallocStr(length);
    
    if (str) {
        strcpy(newStr, str);
        strcat(newStr, addOn);
    } else strcpy(newStr, addOn);
    strcat(newStr, "\n\n");    
    return newStr;
}

static char* addStr(char* str, char* addOn)
{
    int length = (str ? strlen(str) : 0) + strlen(addOn);
    char* newStr = UTIL_mallocStr(length);
    
    if (str) {
        strcpy(newStr, str);
        strcat(newStr, addOn);
    } else strcpy(newStr, addOn);
    return newStr;
}


/**********************************************************************/
/* auxiliary functions for making ocaml language constructs           */
/**********************************************************************/
#define TYPE_SUFFIX      "type"
#define SIZE_SUFFIX      "Size"
#define WRITE_PREFIX     "write"
#define INDENT           "  "
#define INDENT2          "    "
#define WRITE            "Writeutil.write"
#define INSCAT_PREFIX    "inscat"
#define INS_PREFIX       "Ins_"

static char* OC_mkVarDec(char* varName, char* varType)
{
    int length = strlen(varName) + strlen(varType) + 10;    
    char* vardec = UTIL_mallocStr(length);
    
    strcpy(vardec, "val ");
    strcat(vardec, varName);
    strcat(vardec, " : ");
    strcat(vardec, varType);
    strcat(vardec, "\n");
    
    return vardec;
}

static char* OC_mkVarDef(char* varName, char* defs)
{
    int length = strlen(varName) + strlen(defs) + 10;
    char* vardef = UTIL_mallocStr(length);
    
    strcpy(vardef, "let ");
    strcat(vardef, varName);
    strcat(vardef, " = ");
    strcat(vardef, defs);
    strcat(vardef, "\n");
    
    return vardef;
}

static char* OC_mkTypeDec(char* typeName, char* defs)
{
    int length = strlen(typeName) + strlen(defs) + 10;
    char* typedec = UTIL_mallocStr(length);
    
    strcpy(typedec, "type ");
    strcat(typedec, typeName);
    strcat(typedec, " = ");
    strcat(typedec, defs);
    strcat(typedec, "\n");
    
    return typedec;
}

static char* OC_mkFunc(char* funcName, char* arg, char* body)
{
    int length = strlen(funcName) + strlen(arg) + strlen(body) + 20;
    char* func = UTIL_mallocStr(length);
    
    strcpy(func, "let ");
    strcat(func, funcName);
    strcat(func, " ");
    strcat(func, arg);
    strcat(func, " = ");
    strcat(func, body);
    strcat(func, "\n");
    
    return func;
}

static char* OC_mkCrossType(char *lop, char *rop)
{
    int length = strlen(lop) + strlen(rop) + 5;
    char* crossType = UTIL_mallocStr(length);
    
    strcpy(crossType, lop);
    strcat(crossType, " * ");
    strcat(crossType, rop);
    
    return crossType;
}

static char* OC_mkValueCtr(char* ctrName, char* types)
{
    int length = strlen(ctrName) + strlen(types) + 10;
    char* ctr = UTIL_mallocStr(length);
    
    strcpy(ctr, ctrName);
    strcat(ctr, " of ");
    strcat(ctr, types);
    return ctr;
}

static char* OC_mkDisjValueCtrs(char* prev, char* next)
{
    int length = strlen(prev) + strlen(next) + 10;
    char* ctr = UTIL_mallocStr(length);
    
    strcpy(ctr, prev);
    strcat(ctr, "\n");
    strcat(ctr, INDENT);
    strcat(ctr, "| ");
    strcat(ctr, next);
    
    return ctr;
}

static char* OC_mkFuncSeq(char* prev, char* new)
{
    int length = strlen(prev) + strlen(new) + 20;
    char* funcSeq = UTIL_mallocStr(length);
    
    strcpy(funcSeq, prev);
    strcat(funcSeq, "; ");
    strcat(funcSeq, new);
    return funcSeq;
}

static char* OC_mkArgList(char* prev, char* new)
{
  int length = strlen(prev) + strlen(new) + 2;
  char* args = UTIL_mallocStr(length);
  
  strcpy(args, prev);
  strcat(args, ", ");
  strcat(args, new);
  
  return args;
}

static char* OC_mkArrow(char* left, char* right)
{
    int length = strlen(left) + strlen(right) + 20;
    char* arrow = UTIL_mallocStr(length);
    
    strcpy(arrow, left);
    strcat(arrow, " -> ");
    strcat(arrow, right);

    return arrow;
}

static char* OC_mkStructure(char* func, char* arg)
{
    int length = strlen(func) + strlen(arg) + 5;
    char* app = UTIL_mallocStr(length);
    
    strcpy(app, func);
    strcat(app, "(");
    strcat(app, arg);
    strcat(app, ")");

    return app;
}

/**************************************************************************/
/* operand types                                                          */
/**************************************************************************/
static char* opTypes;
static char* opSizesMLI;
static char* opSizesML;
static char* writeFuncs;

static char* ocgenWriteOpFunc(char* typeName, char* compType, int numBytes)
{
    char* funcName = UTIL_appendStr(WRITE_PREFIX, typeName);
    char* numBytesText = UTIL_itoa(numBytes);
    char* arg = "arg";
    char* funcBody1 = UTIL_mallocStr(strlen(WRITE) + strlen(compType) +
                                     strlen(numBytesText));
    char *funcBody2, *func;
    
    strcpy(funcBody1, WRITE);
    strcat(funcBody1, compType);
    strcat(funcBody1, numBytesText);                free(numBytesText);
    
    funcBody2 = UTIL_appendStr(funcBody1, " arg");  free(funcBody1);
    func = OC_mkFunc(funcName, arg, funcBody2); 
    free(funcName); free(funcBody2);
    return func;
}


void ocgenOpType(char* typeName, int numBytes, char* compType)
{
    /* generate type declarations*/
    char* myName        = UTIL_lowerCase(typeName);
    char* myTypeName    = UTIL_appendStr(myName, TYPE_SUFFIX);
    char* myOpType      = OC_mkTypeDec(myTypeName, compType);
    char* myopTypes     = addStr(opTypes, myOpType);
    /* generate write functions */
    char* func          = ocgenWriteOpFunc(typeName, compType, numBytes);
    char* myWriteFuncs  = addStr(writeFuncs, func);

    /* generate sizes */
    if (numBytes < 4) {
        char* mySizeName    = UTIL_appendStr(myName, SIZE_SUFFIX);
        char* myOpSizeMLI   = OC_mkVarDec(mySizeName, "int");
        char* size          = UTIL_itoa((int)(pow(2,(numBytes * 8))-1));
        char* myOpSizeML    = OC_mkVarDef(mySizeName, size);
        char* myopSizesMLI  = addStr(opSizesMLI, myOpSizeMLI);
        char* myopSizesML   = addStr(opSizesML, myOpSizeML);

        free(mySizeName); free(size); free(myOpSizeMLI); free(myOpSizeML);
        free(opSizesMLI); free(opSizesML);
        opSizesMLI =  myopSizesMLI;  
        opSizesML   = myopSizesML;
    }
    free(myTypeName); free(myName);
    free(opTypes); free(myOpType);
    opTypes = myopTypes;
    free(writeFuncs); free(func);
    writeFuncs = myWriteFuncs;
}

void ocgenOpCodeType(int numBytes)
{
    char* mySizeName = UTIL_appendStr("opcode", SIZE_SUFFIX);
    char* size       = UTIL_itoa((int)(pow(2,(numBytes * 8))-1));
    char* myOpCodeSizeMLI = OC_mkVarDec(mySizeName, "int");
    char* myOpCodeSizeML  = OC_mkVarDef(mySizeName, size);
    char* myopSizeMLI = addLine(opSizesMLI, myOpCodeSizeMLI);
    char* myopSizeML  = addLine(opSizesML, myOpCodeSizeML);
    char* func = ocgenWriteOpFunc("opcode", "int", numBytes);
    char* myWriteFuncs = addLine(writeFuncs, func);
    
    free(size); free(mySizeName);
    free(opSizesMLI);   free(myOpCodeSizeMLI);
    free(opSizesML);    free(myOpCodeSizeML);  
    free(writeFuncs);   free(func);
    opSizesMLI = myopSizeMLI;
    opSizesML  = myopSizeML;
    writeFuncs = myWriteFuncs;
}

#define INTREF "type intref = int ref\n"

static char* opMLI;
static char* opML;

void ocgenOps()
{
    char* wordSizeName = "wordSize";
    char* wordSizeMLI  = OC_mkVarDec(wordSizeName, "int");
    char* wordSize     = UTIL_itoa(sizeof(void*));
    char* wordSizeML   = OC_mkVarDef(wordSizeName, wordSize);
    char* myOpTypes    = addLine(INTREF, opTypes);
    char* text;
    
    free(wordSize);  free(opTypes);

    opMLI = addLine(opMLI, wordSizeMLI); free(wordSizeMLI);
    text = addLine(opMLI, opSizesMLI);   free(opMLI); free(opSizesMLI);
    opMLI = addLine(text, myOpTypes);    free(text); 

    opML = addLine(opML, wordSizeML);    free(wordSizeML);
    text = addLine(opML, opSizesML);     free(opML); free(opSizesML);
    opML = addLine(text, writeFuncs);    free(text); free(writeFuncs);
    text = addLine(opML, myOpTypes);     free(opML); free(myOpTypes);
    opML = text;
}

/****************************************************************************/
/* instruction categories                                                   */
/****************************************************************************/
static char* instrCatWriteFunc = NULL;
static char* instrCatType      = NULL;
static int   argInd            = 1;
static char* argList           = NULL;  

void ocgenInstrFormat(char* opName)
{
  char *myop, *myOpName, *myFuncName, *myArgInd, *myFuncCall, *myArg, 
    *myArgList, *myinstrCatWriteFunc, *myinstrCatType;
    
    if (strcmp(opName, "P") == 0 || strcmp(opName, "WP") == 0 || 
        strcmp(opName, "X") == 0) return;

    //type declaration
    myop = UTIL_lowerCase(opName);
    myOpName = UTIL_appendStr(myop, TYPE_SUFFIX);     free(myop);
    if (instrCatType) {
        myinstrCatType = OC_mkCrossType(instrCatType, myOpName);
        free(instrCatType);   free(myOpName);
        instrCatType = myinstrCatType;
    } else instrCatType = myOpName;

    //argument 
    myArgInd = UTIL_itoa(argInd);
    argInd++;
    myArg = UTIL_appendStr("arg", myArgInd);              free(myArgInd);
    //argument list
    if (argList) {
      myArgList = OC_mkArgList(argList, myArg); free(argList);
      argList = myArgList;
    } else argList = myArg;
    //write function
    myFuncName = UTIL_appendStr(WRITE_PREFIX, opName);
    myFuncCall = UTIL_mallocStr(strlen(myFuncName) + strlen(myArg) + 5);
    strcpy(myFuncCall, myFuncName);     free(myFuncName);
    strcat(myFuncCall, " ");
    strcat(myFuncCall, myArg);    
    if (instrCatWriteFunc) {
        myinstrCatWriteFunc = OC_mkFuncSeq(instrCatWriteFunc, myFuncCall);
        free(instrCatWriteFunc);
        instrCatWriteFunc = myinstrCatWriteFunc;
        free(myFuncCall);
    } else instrCatWriteFunc =  myFuncCall;

}

static char* instrCatTypes;
static char* instrCatWriteFuncs;

void ocgenOneInstrCat(char* catName)
{
  char  *mycat, *myCatName, *myInstrCatType, *myInstrCatTypes, *myWriteFuncName,
    *myWriteFunc, *myInstrCatWriteFuncs, *myArgs;

  if (instrCatType) {
      //mycat = UTIL_lowerCase(catName);
    myCatName = UTIL_appendStr(INSCAT_PREFIX, catName); 
    myInstrCatType = OC_mkTypeDec(myCatName, instrCatType);
    myInstrCatTypes = addStr(instrCatTypes, myInstrCatType);

    myWriteFuncName = UTIL_appendStr(WRITE_PREFIX, catName);
    myArgs = UTIL_mallocStr(strlen(argList) + 5);
    strcpy(myArgs, "(");
    strcat(myArgs, argList);           
    strcat(myArgs, ")");
    myWriteFunc = OC_mkFunc(myWriteFuncName, myArgs, instrCatWriteFunc);
    myInstrCatWriteFuncs = addStr(instrCatWriteFuncs, myWriteFunc);
    
    //free(mycat); 
    free(myCatName); free(myWriteFuncName);
    free(myInstrCatType); free(myWriteFunc);
    free(instrCatType); free(instrCatWriteFunc); free(argList);
    free(instrCatTypes); free(instrCatWriteFuncs);

    instrCatType = NULL; instrCatWriteFunc = NULL; argList = NULL; argInd = 1;
    instrCatTypes = myInstrCatTypes; 
    instrCatWriteFuncs = myInstrCatWriteFuncs;
					       
  }
}

static char* instrCatLength;
void ocgenInstrLength(char* varName, char* numBytes)
{
    char* myVarName = UTIL_appendStr(INSCAT_PREFIX, varName);
    char* varDef    = OC_mkVarDef(myVarName, numBytes);
    char* myInstrCatLength = addStr(instrCatLength, varDef);
    
    free(myVarName); free(varDef); free(instrCatLength);
    instrCatLength = myInstrCatLength;
}

static char* instrCatMLI;
static char* instrCatML;

void ocgenInstrCat()
{
  char* text = instrCatTypes;
  char* text2 = addLine(text, "\n");
  
  instrCatMLI = text;
  instrCatML  = addLine(text2, instrCatWriteFuncs); free(instrCatWriteFuncs);
  text = addLine(instrCatML, instrCatLength); free(instrCatML); free(instrCatLength);
  instrCatML = text;
}

/****************************************************************************/
/* instructions                                                             */
/****************************************************************************/
#define GETSIZE_PREFIX "getSize_"
#define WRITEOPCODE    "writeopcode "

static char* instructionTypes;
static char* insWriteFuncBody;
static char* insSizesDec;
static char* insSizesDef;

void ocgenOneInstr(char* opcode, char* insName, char* insCat, char* insLength)
{
    char *myCatName, *myInsName, *myValueCtr, *myInstrTypes;
    char *myInsSizeName, *myInsLength, *mySizeDef, *mySizeDec, *mySizeDefs, 
        *mySizeDecs;
    char *myPattern, *myWriteOpCodeFunc, *myfuncBody, *myFunc, *myInsWriteFuncBody;
    
    /* value constructors for type instruction */
    myInsName = UTIL_appendStr(INS_PREFIX, insName);
    if (strcmp(insCat, "X") == 0) {
        myValueCtr = myInsName;
    } else {
        myCatName = UTIL_appendStr(INSCAT_PREFIX, insCat); 
        myValueCtr = OC_mkValueCtr(myInsName, myCatName);   free(myCatName);
    }
    if (instructionTypes) {
        myInstrTypes = OC_mkDisjValueCtrs(instructionTypes, myValueCtr);
        free(instructionTypes);
        instructionTypes = myInstrTypes;
    } else instructionTypes = myValueCtr;

    /* write function body */
    myWriteOpCodeFunc = UTIL_appendStr(WRITEOPCODE, opcode);
    if (strcmp(insCat, "X") == 0) {
        myPattern  = strdup(myInsName);   free(myInsName);
        myfuncBody = myWriteOpCodeFunc;
    } else {
        char* myWriteArgsName = UTIL_appendStr(WRITE_PREFIX, insCat);
        char* myWriteArgs = UTIL_mallocStr(strlen(myWriteArgsName) + 5);
        myPattern = OC_mkStructure(myInsName, "arg");   free(myInsName);
        strcpy(myWriteArgs, myWriteArgsName);           free(myWriteArgsName);
        strcat(myWriteArgs, " arg");
        myfuncBody = OC_mkFuncSeq(myWriteOpCodeFunc, myWriteArgs); free(myWriteArgs);
    }
    myFunc = OC_mkArrow(myPattern, myfuncBody); free(myPattern); free(myfuncBody);
    if (insWriteFuncBody) {
        myInsWriteFuncBody =  OC_mkDisjValueCtrs(insWriteFuncBody, myFunc);
        free(insWriteFuncBody); free(myFunc);
        insWriteFuncBody = myInsWriteFuncBody;
    } else {
        insWriteFuncBody = UTIL_appendStr(INDENT2, myFunc);
        free(myFunc);
    }

    /* instruction sizes */
    myInsSizeName = UTIL_appendStr(GETSIZE_PREFIX, insName); 
    myInsLength = UTIL_appendStr(INSCAT_PREFIX, insLength); 
    mySizeDef =  OC_mkVarDef(myInsSizeName, myInsLength); free(myInsLength);
    mySizeDec =  OC_mkVarDec(myInsSizeName, "int");       free(myInsSizeName);
    
    mySizeDefs = addStr(insSizesDef, mySizeDef); free(insSizesDef); free(mySizeDef);
    mySizeDecs = addStr(insSizesDec, mySizeDec); free(insSizesDec); free(mySizeDec);
    
    insSizesDef = mySizeDefs;
    insSizesDec = mySizeDecs;
}

#define INSTRTYPE_HEAD "type instruction = "

#define INSTWRITEFUNC_DEF_HEAD "let writeInstruction inst =\n  match inst with\n"
#define INSTWRITEFUNC_DEC "val writeInstruction : instruction -> unit\n"

static char* instrMLI;
static char* instrML;

void ocgenInstr()
{
    char* text = UTIL_appendStr(INSTRTYPE_HEAD, instructionTypes);
    char* text2 = UTIL_appendStr(text, "\n\n");
    
    free(instructionTypes); free(text);

    text = addLine(text2, insSizesDec); free(insSizesDec);
    instrMLI = addLine(text, INSTWRITEFUNC_DEC); free(text);
    
    text =  addLine(text2, insSizesDef); free(text2); free(insSizesDef);
    text2 = addStr(text, INSTWRITEFUNC_DEF_HEAD);    free(text);    
    instrML = addStr(text2, insWriteFuncBody);   free(text2); free(insWriteFuncBody);
}

/****************************************************************************/
/* dump files                                                               */
/****************************************************************************/
/* dump files */
void ocSpitInstructionMLI()
{
    FILE* outFile;
    outFile = UTIL_fopenW("../../compiler/instr.mli");
    fprintf(outFile, opMLI);             free(opMLI);
    fprintf(outFile, instrCatMLI);       free(instrCatMLI);
    fprintf(outFile, "\n\n");
    fprintf(outFile, instrMLI);          free(instrMLI);
    UTIL_fclose(outFile);
}

/* dump files */
void ocSpitInstructionML()
{
    FILE* outFile;
    outFile = UTIL_fopenW("../../compiler/instr.ml");
    fprintf(outFile, opML);              free(opML);
    fprintf(outFile, instrCatML);        free(instrCatML);
    fprintf(outFile, instrML);           free(instrML);
    UTIL_fclose(outFile);
}

