/*****************************************************************************/
/* File pervgen-c.h. This files contains function definitions for generating */
/* files pervasives.h and pervasives.c.                                      */
/*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pervgen-c.h"

//useful macros, functions and data structures

#define  INDENT_LEV1        "    "
#define  INDENT_LEV1_LEN    4
#define  IND_PREFIX            "PERV_"
#define  IND_PREFIX_LEN        5
#define  IND_SUFFIX            "_INDEX"
#define  IND_SUFFIX_LEN        5 


//convert lower case letters in a string to upper case ones
static char* upperCase(char* str)
{
    char *newstr, *tmp;
    newstr = strdup(str);
    tmp = newstr;
    while ((*tmp) != '\0'){
        if ((97 <= (int)*tmp) && ((int)*tmp <= 122))
            *tmp = (char)((int)*tmp - 32);
        tmp++;
    }
    return newstr;
}

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

//dynamic string array type
typedef struct StringArray 
{
  char **array;
  int  length;
} StringArray;


/*****************************************************************************/
/*  pervasives.h                                                             */
/*****************************************************************************/
#define COMMENTS_BEG_H \
"/****************************************************************************/\n/* File pervasives.h.                                                       */ \n/****************************************************************************/"

#define COMPDEF_BEG_H "#ifndef PERVASIVES_H\n#define PERVASIVES_H\n"
#define COMPDEF_END_H "#endif //PERVASIVES_H\n"

#define INCLUDE_H \
"#include \"../simulator/mctypes.h\"      //to be changed                      \n#include \"../simulator/dataformats.h\"  //to be changed  \n"

/* Kind relevant information    */

//comments
#define KIND_COMMENTS_H \
"/****************************************************************************/\n/*   PERVASIVE KIND                                                         */ \n/****************************************************************************/\n"
#define KIND_COMMENTS_H_LEN 300

//PERV_KIND_NUM
#define NUM_KINDS_LEN  160
static char numKinds[NUM_KINDS_LEN] = 
"//total number of pervasive kinds                                             \n#define PERV_KIND_NUM   ";

void genNumKinds(char* num)
{
    strcat(numKinds, num);
    strcat(numKinds, "\n\n");
}

//PERV_KindIndexType
static StringArray kindIndices;

void genKindIndices(int index, char* name, char* indexT, char* comments)
{
    char*  ucName = upperCase(name);
    int    length;
    int    commentLen = comments ? strlen(comments) : 0;
    char*  kindIndex = NULL; //one index
    
    if (index >= kindIndices.length) {
        fprintf(stderr, "kind index exceed total number of kinds\n");
        exit(1);
    }
    length = IND_PREFIX_LEN + IND_SUFFIX_LEN + strlen(name) +
             INDENT_LEV1_LEN*2 + strlen(indexT) + commentLen + 30;
    kindIndex = (char*)malloc(sizeof(char)*(length + 1));
    strcpy(kindIndex, INDENT_LEV1);
    if (comments) {
      strcat(kindIndex, "//");
      strcat(kindIndex, comments);
      strcat(kindIndex, "\n");
      strcat(kindIndex, INDENT_LEV1);
    }
    strcat(kindIndex, IND_PREFIX);
    strcat(kindIndex, ucName);
    strcat(kindIndex, IND_SUFFIX);
    strcat(kindIndex, " = ");
    strcat(kindIndex, indexT);
    if (index != (kindIndices.length - 1))  strcat(kindIndex, ",");
    strcat(kindIndex, "\n");
    free(ucName);
    kindIndices.array[index] = kindIndex;
}

#define KIND_INDEX_TYPE_BEG \
"//indices for predefined sorts and type constructors                          \ntypedef enum PERV_KindIndexType                                                \n{\n"
#define KIND_INDEX_TYPE_BEG_LEN    200

#define KIND_INDEX_TYPE_END        "} PERV_KindIndexType;\n\n"
#define KIND_INDEX_TYPE_END_LEN    30

static char* genKindIndexType()
{
  char *myText = NULL;
  int  i;
  int  length = 0;
  
  for (i = 0; i < kindIndices.length; i++){
    char *text = kindIndices.array[i];
    if (text) length += strlen(text);
  }
  myText = (char*)malloc(sizeof(char)*(KIND_INDEX_TYPE_BEG_LEN +
				       KIND_INDEX_TYPE_END_LEN +
				       length + 1));
  strcpy(myText, KIND_INDEX_TYPE_BEG);
  for (i = 0; i < kindIndices.length; i++){
    char *text = kindIndices.array[i];
    if (text) strcat(myText, text);
    free(text);
  }
  strcat(myText, KIND_INDEX_TYPE_END);
  free(kindIndices.array);
  return myText;
}


//PERV_KindData 
#define KINDDATA_TYPE_DEF \
"//pervasive kind data type                                                    \ntypedef struct                                                                 \n{                                                                              \n    char         *name;                                                        \n    TwoBytes     arity;                                                        \n} PERV_KindData;                                                             \n\n" 
#define KINDDATA_TYPE_DEF_LEN    500

//PERV_kindDataTab
#define KINDDATATAB_DEC \
"//pervasive kind data table (array)                                           \nextern PERV_KindData    PERV_kindDataTab[PERV_KIND_NUM];                     \n\n"
#define KINDDATATAB_DEC_LEN      200

//PERV_gerKindData
#define GETKINDDATA_DEC \
"//pervasive kind data access function                                         \nPERV_KindData PERV_getKindData(int index);                                   \n\n"
#define GETKINDDATA_DEC_LEN      200  

//PERV_copyKindDataTab
#define COPYKINDDATATAB_DEC \
"//pervasive kind table copy function (used in module space initialization)    \n//this functiion relies on the assumption that the pervasive kind data         \n//has the same structure as that of the run-time kind symbol table entries.    \nvoid PERV_copyKindDataTab(PERV_KindData* dst);                               \n\n"
#define COPYKINDDATATAB_DEC_LEN  400

//pervasive kinds relevant info in pervasives.h
static char* kindH = NULL;

void genKindH(int nonEmpty)
{
  if (nonEmpty) {
    char* myText = genKindIndexType();
    kindH = (char*)malloc(sizeof(char)*(KIND_COMMENTS_H_LEN +
			  strlen(myText) + KINDDATA_TYPE_DEF_LEN +
			  NUM_KINDS_LEN + KINDDATATAB_DEC_LEN +
                          GETKINDDATA_DEC_LEN + COPYKINDDATATAB_DEC_LEN+1));
    strcpy(kindH, KIND_COMMENTS_H);
    strcat(kindH, myText);
    strcat(kindH, numKinds);
    strcat(kindH, KINDDATA_TYPE_DEF);
    strcat(kindH, KINDDATATAB_DEC);
    strcat(kindH, GETKINDDATA_DEC);
    strcat(kindH, COPYKINDDATATAB_DEC);
    free(myText);
  } else {
    kindH = (char*)malloc(sizeof(char)*(KIND_COMMENTS_H_LEN +
                          NUM_KINDS_LEN +  KINDDATA_TYPE_DEF_LEN +
                          KINDDATATAB_DEC_LEN + GETKINDDATA_DEC_LEN +
                          COPYKINDDATATAB_DEC_LEN+ 1));
    strcpy(kindH, KIND_COMMENTS_H);
    strcat(kindH, numKinds);
    strcat(kindH, KINDDATA_TYPE_DEF);
    strcat(kindH, KINDDATATAB_DEC);
    strcat(kindH, GETKINDDATA_DEC);
    strcat(kindH, COPYKINDDATATAB_DEC);
  }
}


/* Type skeleton relevant information */

//comments
#define TYSKEL_COMMENTS_H \
"/****************************************************************************/\n/*   TYPE SKELETIONS FOR PERVASIVE CONSTANTS                                */ \n/****************************************************************************/\n\n"
#define TYSKEL_COMMENTS_H_LEN 300

//PERV_TY_SKEL_NUM 
#define NUM_TYSKEL_LEN  160
static char numTySkels[NUM_TYSKEL_LEN] = 
"//total number of type skeletons needed for pervasive constants               \n#define PERV_TY_SKEL_NUM   ";

void genNumTySkels(char* num)
{
    strcat(numTySkels, num);
    strcat(numTySkels, "\n\n");
}

//PERV_TySkelData
#define TYSKELDATA_TYPE_DEF \
"//pervasive type skel data type                                               \ntypedef DF_TypePtr  PERV_TySkelData;                                         \n\n"
#define TYSKELDATA_TYPE_DEF_LEN     200

//PERV_TySkelTab
#define TYSKELTAB_DEC \
"//pervasive type skel table (array)                                           \nextern  PERV_TySkelData   PERV_tySkelTab[PERV_TY_SKEL_NUM];                  \n\n"
#define TYSKELTAB_DEC_LEN           200

//PERV_tySkelTabInit
#define TYSKELTABINIT_DEC \
"//pervasive type skeletons and type skeleton table initialization             \n//Note that type skeltons have to be dynamically allocated, and so does the    \n//info recorded in each entry of the pervasive type skeleton table             \nvoid PERV_tySkelTabInit();                                                   \n\n"
#define TYSKELTABINIT_DEC_LEN       350

//PERV_copyTySkelTab
#define COPYTYSKELTAB_DEC \
"//pervasive tyskel table copy function                                        \nvoid PERV_copyTySkelTab(PERV_TySkelData* dst);                               \n\n"
#define COPYTYSKELTAB_DEC_LEN       200

//pervasive type skeleton relevant info in pervasives.h
static char* tySkelH = NULL;

void genTySkelH()
{
    tySkelH = (char*)malloc(sizeof(char)*(TYSKEL_COMMENTS_H_LEN + 
                            NUM_TYSKEL_LEN + TYSKELDATA_TYPE_DEF_LEN + 
                            TYSKELTAB_DEC_LEN + TYSKELTABINIT_DEC_LEN +
                            COPYTYSKELTAB_DEC_LEN) + 20);
    strcpy(tySkelH, TYSKEL_COMMENTS_H);
    strcat(tySkelH, numTySkels);
    strcat(tySkelH, TYSKELDATA_TYPE_DEF);
    strcat(tySkelH, TYSKELTAB_DEC);
    strcat(tySkelH, TYSKELTABINIT_DEC);
    strcat(tySkelH, COPYTYSKELTAB_DEC);
}


/* Constant relevant information in pervasives.h */

//comments
#define CONST_COMMENTS_H \
"/***************************************************************************/ \n/*   PERVASIVE CONSTANTS                                                   */  \n/***************************************************************************/\n\n"
#define CONST_COMMENTS_H_LEN 300

//PERV_CONST_NUM
#define NUM_CONSTS_LEN  160
static char numConsts[NUM_CONSTS_LEN] = 
"//total number of pervasive consts                                            \n#define PERV_CONST_NUM   ";

void genNumConsts(char* num)
{
    strcat(numConsts, num);
    strcat(numConsts, "\n\n");
}

//PERV_ConstIndexType

static StringArray constIndices;

void genConstIndices(int index, char* name, char* indexT, char* comments)
{
  char* ucName = upperCase(name);
  int   length;
  int   commentLen = comments ? strlen(comments) : 0;
  char* constIndex = NULL; //one index

  if (index >= constIndices.length) {
            fprintf(stderr, "const index exceed total number of consts\n");
        exit(0);
  }
  length = IND_PREFIX_LEN + IND_SUFFIX_LEN + strlen(name) +
           INDENT_LEV1_LEN*2 + strlen(indexT) + commentLen + 30;
  constIndex = (char*)malloc(sizeof(char)*(length + 1));
  strcpy(constIndex, INDENT_LEV1);
  if (comments){
    strcat(constIndex, "//");
    strcat(constIndex, comments);
    strcat(constIndex, "\n");
    strcat(constIndex, INDENT_LEV1);
  }
  strcat(constIndex, IND_PREFIX);
  strcat(constIndex, ucName);
  strcat(constIndex, IND_SUFFIX);
  strcat(constIndex, " = ");
  strcat(constIndex, indexT);
  //if (index != (constIndices.length - 1)) strcat(constIndex, ",");
  //strcat(constIndex, "\n");
  strcat(constIndex, ",\n");
  free(ucName);
  constIndices.array[index] = constIndex;
}

#define EMPTY_INDEX_COMMENT          "    //empty\n"

void genConstEmptyIndices(int index)
{
  char* constIndex = NULL; //one index

  if (index >= constIndices.length) {
            fprintf(stderr, "const index exceed total number of consts\n");
        exit(0);
  }
  constIndex = strdup(EMPTY_INDEX_COMMENT);
  constIndices.array[index] = constIndex;  
}
 
#define CONST_INDEX_TYPE_BEG \
"//indices for predefined constants                                            \ntypedef enum PERV_ConstIndexType                                               \n{\n"
#define CONST_INDEX_TYPE_BEG_LEN    200

#define CONST_INDEX_TYPE_END        "} PERV_ConstIndexType;\n\n"
#define CONST_INDEX_TYPE_END_LEN    50 


static char* genConstIndexType()
{
  char* myText = NULL;
  int   i;
  int   length = 0;
  int   empty = 1;

  for (i = 0; i < constIndices.length; i++){
    char *text = constIndices.array[i];
    if (text) length += strlen(text);
  }
  i = constIndices.length - 1;
  while ((i > 0) && empty){
    char *text = constIndices.array[i];
    if ((text) && (strcmp(text, EMPTY_INDEX_COMMENT) == 0)) i--;
    else empty = 0;
  }
  if (i == -1) {
    myText = strdup("\n");
    for (i = 0; i < constIndices.length; i++){
      char *text = constIndices.array[i];
      if (text) free(text);
    }
    free(constIndices.array);
  } else {
    char *text  = NULL;
    text = strdup(constIndices.array[i]);
    text[strlen(text)-2] = ' ';
    free(constIndices.array[i]);
    constIndices.array[i] = text;
    myText = (char*)malloc(sizeof(char)*(CONST_INDEX_TYPE_BEG_LEN +
					 CONST_INDEX_TYPE_END_LEN +
					 length + 1));
    strcpy(myText, CONST_INDEX_TYPE_BEG);
    for (i = 0; i < constIndices.length; i++){
      char *text = constIndices.array[i];
      if (text) {
          strcat(myText, text);
          free(text);
      }
    }
    strcat(myText, CONST_INDEX_TYPE_END);
    free(constIndices.array);
  }
  return myText;
}

//PERV_ConstData
#define CONSTDATA_TYPE \
"//pervasive const data type                                                   \ntypedef struct                                                                 \n{                                                                              \n    char      *name;                                                           \n    TwoBytes  typeEnvSize;                                                     \n    TwoBytes  tskTabIndex;     //index to the type skeleton table              \n    //word      neednessVec;   //needness vector (predicate constant)          \n    TwoBytes  univCount;                                                       \n    int       precedence;                                                      \n    int       fixity;                                                          \n} PERV_ConstData;                                                          \n\n"
#define CONSTDATA_TYPE_LEN     900

//PERV_ConstData
#define CONSTDATA_TAB_DEC \
"//pervasive const data table (array)                                          \nextern PERV_ConstData    PERV_constDataTab[PERV_CONST_NUM];                \n\n"
#define CONSTDATA_TAB_DEC_LEN  200

//PERV_getConstData
#define GETCONSTDATA_DEC  \
"//pervasive const data access function                                        \nPERV_ConstData PERV_getConstData(int index);                               \n\n"
#define GETCONSTDATA_DEC_LEN   200

//PERV_copyConstDataTab
#define COPYCONSTDATATAB_DEC \
"//pervasive const table copy function (used in module space initialization)   \n//this functiion relies on the assumption that the pervasive kind data         \n//has the same structure as that of the run-time kind symbol table entries.    \nvoid PERV_copyConstDataTab(PERV_ConstData* dst);                           \n\n"
#define COPYCONSTDATATAB_DEC_LEN 350

//PERV_isLogicSymb PERV_isPredSymb
#define ISLS_ISPS_DEC \
"//functions used by the simulator for interpreted goals                       \nBoolean PERV_isLogicSymb(int index);                                           \nBoolean PERV_isPredSymb(int index);                                        \n\n"
#define ISLS_ISPS_DEC_LEN      250

//PERV_LogicSymbTypes 
static StringArray logicSymbTypes;

void genLogicSymbTypes(int index, char* name, char* indexT)
{
    char* ucName = upperCase(name);
    char* logicSymbType = NULL;

    if (index >= logicSymbTypes.length){
        fprintf(stderr, "logic symbol type index exceed the total number of logic symbols\n");
        exit(1);
    }
    logicSymbType = (char*)malloc(sizeof(char)*(INDENT_LEV1_LEN + strlen(name) +
                                                IND_PREFIX_LEN + strlen(indexT)+
                                                10));
    strcpy(logicSymbType, INDENT_LEV1);
    strcat(logicSymbType, IND_PREFIX);
    strcat(logicSymbType, ucName);
    strcat(logicSymbType, "  = ");
    strcat(logicSymbType, indexT);
    if (index != (logicSymbTypes.length -1)) strcat(logicSymbType, ",");        
    strcat(logicSymbType, "\n");
    free(ucName);

    logicSymbTypes.array[index] = logicSymbType;
}    
    
#define LOGICSYMBTYPE_DEC_BEG \
"typedef enum PERV_LogicSymbTypes                                              \n{\n"
#define LOGICSYMBTYPE_DEC_BEG_LEN     150
#define LOGICSYMBTYPE_DEC_END "} PERV_LogicSymbTypes;\n\n"
#define LOGICSYMBTYPE_DEC_END_LEN 80

static char* genLogicSymbTypeDec()
{
    char *myText = NULL;
    int   i;
    int   length = 0;
    //it is assumned that the number of logic symbol tables are larger than 0
    for (i = 0; i < logicSymbTypes.length; i++){
        char *text = logicSymbTypes.array[i];
        if (text) length += strlen(text);
    }
    myText = (char*)malloc(sizeof(char)*(LOGICSYMBTYPE_DEC_BEG_LEN +
                                         LOGICSYMBTYPE_DEC_END_LEN +
                                         length + 1));
    strcpy(myText, LOGICSYMBTYPE_DEC_BEG);
    for (i = 0; i < logicSymbTypes.length; i++){
        char *text = logicSymbTypes.array[i];
        if (text) strcat(myText, text);
        free(text);
    }
    strcat(myText, LOGICSYMBTYPE_DEC_END);
    free(logicSymbTypes.array);
    return myText;
}

//PERV_LSSTART
#define LSSTART_BEG "#define PREV_LSSTART     "
#define LSSTART_BEG_LEN 20

#define LSSTART_END "     //begin of interpretable symbols\n"
#define LSSTART_END_LEN 50

#define LSEND_BEG   "#define PREV_LSEND       "
#define LSEND_BEG_LEN   20

#define LSEND_END   "     //end of interpretable symbols\n"
#define LSEND_END_LEN   50

static char* lsRange=NULL;

void genLSRange(char* start, char* end)
{
    char* ucStart = upperCase(start);
    char* ucEnd   = upperCase(end);
    lsRange = (char*)malloc(sizeof(char)*(IND_PREFIX_LEN*2 + LSSTART_BEG_LEN +
                            LSSTART_END_LEN + LSEND_BEG_LEN + LSEND_END_LEN +
                            strlen(start) + strlen(end) + 2 +
                            IND_SUFFIX_LEN*2));
    strcpy(lsRange, LSSTART_BEG);
    strcat(lsRange, IND_PREFIX);
    strcat(lsRange, ucStart);
    strcat(lsRange, IND_SUFFIX);
    strcat(lsRange, LSSTART_END);
    strcat(lsRange, LSEND_BEG);
    strcat(lsRange, IND_PREFIX);
    strcat(lsRange, ucEnd);
    strcat(lsRange, IND_SUFFIX);
    strcat(lsRange, LSEND_END);
    strcat(lsRange, "\n");
}

//PERV_PREDSTART
#define PREDSTART_BEG "#define PREV_PREDSTART     "
#define PREDSTART_BEG_LEN 20

#define PREDSTART_END "     //begin of predicate symbols\n"
#define PREDSTART_END_LEN 50

#define PREDEND_BEG   "#define PREV_PREDEND       "
#define PREDEND_BEG_LEN 20

#define PREDEND_END   "     //end of predicate symbols\n"
#define PREDEND_END_LEN 50

static char* predRange=NULL;

void genPREDRange(char* start, char* end)
{
    char* ucStart = upperCase(start);
    char* ucEnd   = upperCase(end);
    predRange = (char*)malloc(sizeof(char)*(IND_PREFIX_LEN*2 +PREDSTART_BEG_LEN+
                            PREDSTART_END_LEN + PREDEND_BEG_LEN +
                            PREDEND_END_LEN + strlen(start) + strlen(end) + 2 +
                            IND_SUFFIX_LEN*2));
    strcpy(predRange, PREDSTART_BEG);
    strcat(predRange, IND_PREFIX);
    strcat(predRange, ucStart);
    strcat(predRange, IND_SUFFIX);
    strcat(predRange, PREDSTART_END);
    strcat(predRange, PREDEND_BEG);
    strcat(predRange, IND_PREFIX);
    strcat(predRange, ucEnd);
    strcat(predRange, IND_SUFFIX);
    strcat(predRange, PREDEND_END);
    strcat(predRange, "\n");
}

//PERV_logicSymb
#define LOGICSYMB_DEC "PERV_LogicSymbTypes PERV_logicSymb(int index); \n\n"
#define LOGICSYMB_DEC_LEN      80   

//pervasive consts relevant info in pervasives.h
char* constH = NULL;

void genConstH()
{
    char* myText1 = genLogicSymbTypeDec(); 
    char* myText  = genConstIndexType();
    int length = CONST_COMMENTS_H_LEN + NUM_CONSTS_LEN +
        CONSTDATA_TYPE_LEN + CONSTDATA_TAB_DEC_LEN + GETCONSTDATA_DEC_LEN + 
        COPYCONSTDATATAB_DEC_LEN + ISLS_ISPS_DEC_LEN + LOGICSYMB_DEC_LEN + 
        strlen(myText1) + strlen(lsRange) + strlen(predRange) + strlen(myText);

    constH = (char*)malloc(sizeof(char)*(length + 1)); 
    strcpy(constH, CONST_COMMENTS_H);
    strcat(constH, myText);              free(myText);
    strcat(constH, numConsts);
    strcat(constH, CONSTDATA_TYPE);
    strcat(constH, lsRange);             free(lsRange);
    strcat(constH, predRange);           free(predRange);
    strcat(constH, CONSTDATA_TAB_DEC);
    strcat(constH, GETCONSTDATA_DEC);
    strcat(constH, COPYCONSTDATATAB_DEC);
    strcat(constH, myText1);             free(myText1);
    strcat(constH, ISLS_ISPS_DEC);
    strcat(constH, LOGICSYMB_DEC);
}


/* dump peravsives.h   */
void spitCPervasivesH()
{
    FILE* outFile;
    outFile = fopen("../tables/pervasives.h", "w");
    if (outFile) {
        fprintf(outFile, "%s\n%s\n", COMPDEF_BEG_H, INCLUDE_H);
        fprintf(outFile, "%s\n", kindH);
        fprintf(outFile, "%s\n", tySkelH);
        fprintf(outFile, "%s\n", constH);
        fprintf(outFile, COMPDEF_END_H);
        fclose(outFile);
    } else printf("can not open\n");
    free(kindH); free(tySkelH); free(constH);
}

/*****************************************************************************/
/*  pervasives.c                                                             */
/*****************************************************************************/

#define COMMENTS_BEG_C  \
"/***************************************************************************/\n /* File pervasives.c.                                                      */\n  /***************************************************************************/\n\n"

#define COMPDEF_BEG_C  "#ifndef PERVASIVES_C\n#define PERVASIVES_C\n"
#define COMPDEF_END_C  "#endif //PERVASIVES_C\n"

#define INCLUDE_C \
"#include <string.h>                                                           \n#include \"pervasives.h\"                                                      \n#include \"../system/error.h\"     //to be changed                             \n#include \"../system/operators.h\" //to be changed                           \n\n"

/*Kind relevant information */

//comments
#define KIND_COMMENTS_C \
"/****************************************************************************/\n/*   PERVASIVE KIND                                                         */ \n/****************************************************************************/\n"
#define KIND_COMMENTS_C_LEN    300


//PERV_kindDataTab
static StringArray kindData;

void genKindData(int index, char* name, char* arity, char* comments)
{
  int   length;
  int   commentLen = comments ? strlen(comments) : 0;
  char* oneKindData = NULL;
  
  if (index >= kindData.length){
      fprintf(stderr, "kind index exceed total number of kinds\n");
      exit(1);
  }
  length = INDENT_LEV1_LEN*2 + commentLen + strlen(name) + strlen(arity) + 50;
  oneKindData = (char*)malloc(sizeof(char)*(length +1));
  strcpy(oneKindData, INDENT_LEV1);
  if (comments) {
    strcat(oneKindData, "//");
    strcat(oneKindData, comments);
    strcat(oneKindData, "\n");
    strcat(oneKindData, INDENT_LEV1);
  }
  strcat(oneKindData, "{\"");
  strcat(oneKindData, name);
  strcat(oneKindData, "\",     ");
  strcat(oneKindData, arity);
  strcat(oneKindData, "}");
  if (index != kindData.length) strcat(oneKindData, ",");
  strcat(oneKindData, "\n");
  kindData.array[index] = oneKindData;
}


#define KINDDATA_TAB_BEG \
"//pervasive kind data table (array)                                           \nPERV_KindData   PERV_kindDataTab[PERV_KIND_NUM] = {                            \n   //name,            arity                                                    \n"
#define KINDDATA_TAB_BEG_LEN   300
#define KINDDATA_TAB_END       "};\n\n"
#define KINDDATA_TAB_END_LEN   4

static char* genKindDataTab()
{
  char *myText = NULL;
  int   i;
  int   length = 0;

  for (i = 0; i < kindData.length; i++){
    char *text = kindData.array[i];
    if (text) length += strlen(text);
    else {
      fprintf(stderr, "empty kind data table entry at %d\n", i);
      exit(1);
    }
  }
  myText = (char*)malloc(sizeof(char)*(KINDDATA_TAB_BEG_LEN +
				         KINDDATA_TAB_END_LEN + length + 1));
  strcpy(myText, KINDDATA_TAB_BEG);
  for ( i = 0; i < kindData.length; i++){
    //now assume empty entries all have been catched before
    strcat(myText, kindData.array[i]);
    free(kindData.array[i]);
  } //note it's possible there is no entries in this tab
  strcat(myText, KINDDATA_TAB_END);
  free(kindData.array);
  return myText;
}


//PERV_getKindData
#define GETKINDDATA_DEF \
"PERV_KindData PERV_getKindData(int index)                                     \n{                                                                              \n    return PERV_kindDataTab[index];                                            \n}                                                                            \n\n"
#define GETKINDDATA_DEF_LEN      400

//PERV_copyKindDataTab
#define COPYKINDDATATAB_DEF \
"void PERV_copyKindDataTab(PERV_KindData* dst)                                 \n{                                                                              \n    //this way of copy relies on the assumption that the pervasive kind data   \n    //has the same structure as that of the run-time kind symbol table entries.\n    memcpy((void*)dst, (void*)PERV_kindDataTab,                                \n           sizeof(PERV_KindData) * PERV_KIND_NUM);                             \n}                                                                            \n\n"
#define COPYKINDDATATAB_DEF_LEN  600

//pervasive kinds relevant info in pervasives.c
static char* kindC = NULL;

void genKindC()
{
  char *myText = genKindDataTab();
  kindC = (char*)malloc(sizeof(char)*(KIND_COMMENTS_C_LEN + strlen(myText) +
                                      GETKINDDATA_DEF_LEN + 
                                      COPYKINDDATATAB_DEF_LEN + 1));
  strcpy(kindC, KIND_COMMENTS_C);
  strcat(kindC, myText);
  strcat(kindC, GETKINDDATA_DEF);
  strcat(kindC, COPYKINDDATATAB_DEF);
  free(myText);
}


/*Type skeleton relevant information*/

//comments
#define TYSKEL_COMMENTS_C \
"/****************************************************************************/\n/*   TYPE SKELETIONS FOR PERVASIVE CONSTANTS                                */ \n/****************************************************************************/\n\n"
#define TYSKEL_COMMENTS_C_LEN     300

//PERV_tySkelTab
#define TYSKELTAB_DEF     \
"//pervasive type skeleton table (array)                                       \nPERV_TySkelData   PERV_tySkelTab[PERV_TY_SKEL_NUM];                        \n\n"
#define TYSKELTAB_DEF_LEN         200

//PERV_tySkelTabInit

//manipulating each type skeleton
#define MKSKVARTYPE_BEG "    DF_mkSkelVarType(tySkelBase, "
#define MKSKVARTYPE_BEG_LEN 30

#define MKSORTTYPE_BEG  "    DF_mkSortType(tySkelBase, "
#define MKSORTTYPE_BEG_LEN  30

#define MKATOMTYPE_END  ");\n"
#define MKATOMTYPE_END_LEN  5

#define MKARROWTYPE_BEG "    DF_mkArrowType(tySkelBase, (DF_TypePtr)(tySkelBase + "
#define MKARROWTYPE_BEG_LEN 80

#define MKSTRTYPE_BEG  "    DF_mkStrType(tySkelBase, (DF_TypePtr)(tySkelBase + "
#define MKSTRTYPE_BEG_LEN   80   

#define MKSTRFUNCTYPE_BEG "    DF_mkStrFuncType(tySkelBase, "
#define MKSTRFUNCTYPE_BEG_LEN 50

#define MKCOMPTYPE_END " * DF_TY_ATOMIC_SIZE));\n"
#define MKCOMPTYPE_END_LEN  30

#define TYSKELBASE_INC "    tySkelBase += DF_TY_ATOMIC_SIZE;\n"
#define TYSKELBASE_INC_LEN  50

static char* genTySkelSort(char* name)
{
    char* mytext = NULL;
    mytext = (char*)malloc(sizeof(char)*(MKSORTTYPE_BEG_LEN+MKATOMTYPE_END_LEN +
                                         strlen(name) + IND_PREFIX_LEN +
                                         IND_SUFFIX_LEN + 
                                         TYSKELBASE_INC_LEN + 1));
    strcpy(mytext, MKSORTTYPE_BEG);
    strcat(mytext, IND_PREFIX);
    strcat(mytext, name);
    strcat(mytext, IND_SUFFIX);
    strcat(mytext, MKATOMTYPE_END);
    strcat(mytext, TYSKELBASE_INC);
    free(name);
    return mytext;
}

static char* genTySkelSkVar(char* index)
{
    char* mytext = NULL;
    mytext = (char*)malloc(sizeof(char)*(MKSKVARTYPE_BEG_LEN+MKATOMTYPE_END_LEN+
                                         strlen(index) + TYSKELBASE_INC_LEN+1));
  
    strcpy(mytext, MKSKVARTYPE_BEG);
    strcat(mytext, index);
    strcat(mytext, MKATOMTYPE_END);
    strcat(mytext, TYSKELBASE_INC);
    return mytext;
}

static char* genTySkelFunc(char* name, char* arity)
{
    char* mytext = NULL;
    mytext = (char*)malloc(sizeof(char)*(MKSTRFUNCTYPE_BEG_LEN + strlen(name) +
                                         strlen(arity) + IND_PREFIX_LEN +
                                         IND_SUFFIX_LEN + 
                                         MKATOMTYPE_END_LEN +
                                         TYSKELBASE_INC_LEN+5));
    strcpy(mytext, MKSTRFUNCTYPE_BEG);
    strcat(mytext, IND_PREFIX);
    strcat(mytext, name);
    strcat(mytext, IND_SUFFIX);
    strcat(mytext, ", ");
    strcat(mytext, arity);
    strcat(mytext, MKATOMTYPE_END);
    strcat(mytext, TYSKELBASE_INC);
    free(name);
    return mytext;
}

static char* genTySkelArrow(char* argPos)
{
    char* mytext = NULL;
    mytext = (char*)malloc(sizeof(char)*(MKARROWTYPE_BEG_LEN + strlen(argPos) +
                           MKCOMPTYPE_END_LEN+TYSKELBASE_INC_LEN+1));
    strcpy(mytext, MKARROWTYPE_BEG);
    strcat(mytext, argPos);
    strcat(mytext, MKCOMPTYPE_END);
    strcat(mytext, TYSKELBASE_INC);
    free(argPos);
    return mytext;
}

static char* genTySkelStr(char* argPos)
{
    char* mytext = NULL;
    mytext = (char*)malloc(sizeof(char)*(MKSTRTYPE_BEG_LEN + strlen(argPos) +
                           MKCOMPTYPE_END_LEN + TYSKELBASE_INC_LEN + 1));
    strcpy(mytext, MKSTRTYPE_BEG);
    strcat(mytext, argPos);
    strcat(mytext, MKCOMPTYPE_END);
    strcat(mytext, TYSKELBASE_INC);
    free(argPos);
    return mytext;
}
    

//data structure used for breath-first traversal of type skels
typedef struct Types
{
    int      length;
    TypeList types;
} Types;

static int    totalSpace = 0;

static char* genOneTySkel(Types types)
{
    TypeList typeList = types.types;
    int      length   = types.length;
    char*    mytext   = NULL;

    if (length){
        Type     myType    = typeList -> oneType;
        TypeList remaining = typeList -> next;
        char *mytext1 = NULL, *mytext2 = NULL;
        free(typeList);
        switch (myType -> tag){
        case SORT:
        {
            mytext1 = genTySkelSort(upperCase(myType->data.sort));
            totalSpace++;
            types.types = remaining;
            types.length = length-1;
            mytext2 = genOneTySkel(types);
            break;
        }
        case SKVAR:
        {
            mytext1 = genTySkelSkVar(myType->data.sort);
            totalSpace++;
            types.types = remaining;
            types.length = length-1;
            mytext2 = genOneTySkel(types);
            break;
        }
        case FUNC:
        {
            mytext1 = genTySkelFunc(upperCase(myType->data.func.name), 
                                    myType->data.func.arity);
            totalSpace++;
            types.types = remaining;
            types.length = length -1;
            mytext2 = genOneTySkel(types); 
            break;
        }
        case ARROW:
        {            
            Type lop = myType->data.arrow.lop;
            Type rop = myType->data.arrow.rop;
            mytext1 = genTySkelArrow(itoa(length));
            totalSpace++;
            remaining = addItemToEnd(remaining, lop);
            types.types = addItemToEnd(remaining, rop);
            types.length = length+1;
            mytext2 = genOneTySkel(types);
            break;
        }
        case STR:
        {
            Type     func = myType->data.str.functor;
            TypeList args = myType->data.str.args;
            int      arity = myType -> data.str.arity;
            mytext1 = genTySkelStr(itoa(length));
            totalSpace++;
            remaining = addItemToEnd(remaining, func);
            types.types = append(remaining, args);
            types.length = length + arity;
            mytext2 = genOneTySkel(types);
            break;
        }
        }
        freeType(myType);
        mytext = (char*)malloc(sizeof(char)*(strlen(mytext1) + 
                                             strlen(mytext2) + 1));
        strcpy(mytext, mytext1); free(mytext1);
        strcat(mytext, mytext2); free(mytext2);
    } else {
        mytext = strdup("");
    }  
    return mytext;
}

#define TYSKELS_PRE \
"    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;                  \n    tySkelInd++;\n"
#define TYSKELS_PRE_LEN           200

static StringArray tySkels;

void genTySkels(int index, Type tySkelAbs, char* comments)
{
    int   length;
    int   commentLen = comments ? strlen(comments) : 0;
    char* tySkelText = NULL;
    char* oneTySkel  = NULL;
    Types types;

    if (index >= tySkels.length){
        fprintf(stderr, 
                "type skeleton index exceed total number of type skeletons\n");
        exit(1);
    }

    types.length = 1;
    types.types  = addItem(tySkelAbs, NULL);
    tySkelText = genOneTySkel(types);

    length = INDENT_LEV1_LEN*2 + commentLen + strlen(tySkelText) +
             TYSKELS_PRE_LEN + 20;
    oneTySkel = (char*)malloc(sizeof(char)*(length + 1));
    strcpy(oneTySkel, "");
    if (comments) {
        strcat(oneTySkel, INDENT_LEV1);
        strcat(oneTySkel, "//");
        strcat(oneTySkel, comments);
        strcat(oneTySkel, "\n");
    }
    strcat(oneTySkel, TYSKELS_PRE);
    strcat(oneTySkel, tySkelText);
    strcat(oneTySkel, "\n");
    free(tySkelText);
    tySkels.array[index] = oneTySkel;
}

#define TYSKELTABINIT_BEG \
"//pervasive type skeletons and type skeleton table initialization             \n//The type skeletons are created in the memory of the system through malloc,   \n//and addresses are entered into the pervasive type skeleton table.            \nvoid PERV_tySkelTabInit()                                                      \n{                                                                              \n    int tySkelInd = 0; //ts tab index\n"
#define TYSKELTABINIT_BEG_LEN     500

#define TYSKELTABINIT_END "}\n\n"
#define TYSKELTABINIT_END_LEN     5

static char* genTySkelTabInit()
{
    char *spaceAlloc = NULL;
    char *spaceText  = itoa(totalSpace*2);
    char *myText = NULL;
    int  i;
    int  length = 0;

    for (i = 0; i < tySkels.length; i++){
        char *text = tySkels.array[i];
        if (text) length += strlen(text);
        else { 
            fprintf(stderr, "empty type skeleton table entry at %d\n", i);
            exit(1);
        }
    }

    //it is assumed that the number of ty skels is larger than 0
    spaceAlloc = (char*)malloc(sizeof(char)*(strlen(spaceText) + 80));
    strcpy(spaceAlloc, 
           "    MemPtr tySkelBase = (MemPtr)EM_malloc(WORD_SIZE * ");
    strcat(spaceAlloc, spaceText);
    strcat(spaceAlloc, " ); //ts area\n\n");
    free(spaceText);
        
    myText = (char*)malloc(sizeof(char)*(TYSKELTABINIT_BEG_LEN + length +
                           TYSKELTABINIT_END_LEN + strlen(spaceAlloc) +  1));
    strcpy(myText, TYSKELTABINIT_BEG);
    strcat(myText, spaceAlloc);    free(spaceAlloc);
    for (i = 0; i < tySkels.length; i++){
        //now assume empty entries all have been catched before
        strcat(myText, tySkels.array[i]);
        free(tySkels.array[i]);
    }
    strcat(myText, TYSKELTABINIT_END);
    free(tySkels.array);
    return myText;
}

//PERV_copyTySkelTab
#define COPYTYSKELTAB_DEF \
"void PERV_copyTySkelTab(PERV_TySkelData* dst)                                 \n{                                                                              \n    memcpy((void*)dst, (void*)PERV_tySkelTab,                                  \n           sizeof(PERV_TySkelData) * PERV_KIND_NUM);                           \n}\n\n"
#define COPYTYSKELTAB_DEF_LEN 320

char* tySkelC = NULL;

void genTySkelC()
{
    char* myText = genTySkelTabInit();
    tySkelC = (char*)malloc(sizeof(char)*(TYSKEL_COMMENTS_C_LEN +
                                          TYSKELTAB_DEF_LEN  + strlen(myText) + 
                                          COPYTYSKELTAB_DEF_LEN + 1));
    strcpy(tySkelC, TYSKEL_COMMENTS_C);
    strcat(tySkelC, TYSKELTAB_DEF);
    strcat(tySkelC, myText);
    strcat(tySkelC, COPYTYSKELTAB_DEF);
    free(myText);
}


/* Constant relevant information in pervasives.c      */

//comments
#define CONST_COMMENTS_C \
"/****************************************************************************/\n/*   PERVASIVE CONSTANTS                                                    */ \n/****************************************************************************/\n\n"
#define CONST_COMMENTS_C_LEN 300

//PERV_ConstData
static StringArray constData;

void genConstData(int index, char* name, char* tesize, char* prec, char* fixity,
		  int tySkelInd, char* comments)
{
  int   length;
  int   commentLen = comments ? strlen(comments) : 0;
  char* tySkelIndT = itoa(tySkelInd);
  char* oneConstData = NULL;

  if (index >= constData.length){
    fprintf(stderr, "const index exceed total number of consts\n");
    exit(1);
  }
  length = INDENT_LEV1_LEN*2 + strlen(name) + strlen(tesize) + strlen(prec) +
    commentLen + strlen(tySkelIndT) + 50;
  oneConstData = (char*)malloc(sizeof(char)*(length + 1));
  
  strcpy(oneConstData, INDENT_LEV1);
  if (comments) {
    strcat(oneConstData, "//");
    strcat(oneConstData, comments);
    strcat(oneConstData, "\n");
    strcat(oneConstData, INDENT_LEV1);
  }
  
  strcat(oneConstData, "{\"");
  strcat(oneConstData, name);
  strcat(oneConstData, "\",     ");
  strcat(oneConstData, tesize);
  strcat(oneConstData, ",     ");
  strcat(oneConstData, tySkelIndT);
  strcat(oneConstData, ",     ");
  strcat(oneConstData, "0,  ");
  strcat(oneConstData, prec);
  strcat(oneConstData, ",   ");
  strcat(oneConstData, fixity);
  strcat(oneConstData, "  }");
  if (index != (constData.length - 1)) strcat(oneConstData, ",");
  strcat(oneConstData, "\n");
  
  constData.array[index] = oneConstData;
}

#define EMPTY_CONSTDATA


void genConstEmptyData(int index)
{
  char* oneConstData = NULL;

  if (index >= constData.length){
    fprintf(stderr, "const index exceed total number of consts\n");
    exit(1);
  }
  
  if (index != constData.length - 1) {
    oneConstData = 
      strdup("    // <nothing>                                                 \n    {NULL,   0,     0,     0,     0,   OP_NONE  },\n");
  } else 
    oneConstData = 
      strdup("    // <nothing>                                                 \n    {NULL,   0,     0,     0,     0,   OP_NONE  }\n");

  constData.array[index] = oneConstData;
}

#define CONSTDATA_TAB_BEG \
"//pervasive constant data table (array)                                       \nPERV_ConstData   PERV_constDataTab[PERV_CONST_NUM] = {                         \n    //name,   tesize, tst, (need), UC, prec,          fixity                 \n"
#define CONSTDATA_TAB_BEG_LEN  300

#define CONSTDATA_TAB_END "};\n\n"
#define CONSTDATA_TAB_END_LEN   5

static char* genConstDataTab()
{
  char *myText = NULL;
  int   i;
  int   length = 0;

  for (i = 0; i < constData.length; i++){
    char *text = constData.array[i];
    if (text) length += strlen(text);
    else {
      fprintf(stderr, "empty const data table entry at %d\n", i);
      exit(1);
    }
  }
  myText = (char*)malloc(sizeof(char)*(CONSTDATA_TAB_BEG_LEN +
				       CONSTDATA_TAB_END_LEN + length + 1));
  strcpy(myText, CONSTDATA_TAB_BEG);
  for (i = 0; i < constData.length; i++){
    //now assume empty entries all have been catched before
    strcat(myText, constData.array[i]);
    free(constData.array[i]);
  } 
  strcat(myText, CONSTDATA_TAB_END);
  free(constData.array);
  return myText;
}

//PERV_getConstData
#define GETCONSTDATA_DEF \
"PERV_ConstData PERV_getConstData(int index)                                   \n{                                                                              \n        return PERV_constDataTab[index];                                       \n}                                                                          \n\n"
#define GETCONSTDATA_DEF_LEN 300

//PERV_copyConstDataTab
#define COPYCONSTDATATAB_DEF \
"void PERV_copyConstDataTab(PERV_ConstData* dst)                               \n{                                                                              \n    //this way of copy relies on the assumption that the pervasive kind data   \n    //has the same structure as that of the run-time kind symbol table entries.\n    memcpy((void*)dst, (void*)PERV_constDataTab,                               \n           sizeof(PERV_ConstData) * PERV_CONST_NUM);                           \n}                                                                          \n\n"
#define COPYCONSTDATATAB_DEF_LEN 600

//PERV_isLogicSymb
#define ISLOGICSYMB_DEF \
"Boolean PERV_isLogicSymb(int index)                                           \n{                                                                              \n    return ((index >= PREV_LSSTART) && (index <= PREV_LSEND));                  \n}\n\n"
#define ISLOGICSYMB_DEF_LEN 350

 
//PERV_isPredSymb
#define ISPREDSYMB_DEF \
"Boolean PERV_isPredSymb(int index)                                            \n{                                                                              \n    return ((index >= PREV_PREDSTART) && (index <= PREV_PREDSTART));           \n}\n\n"
#define ISPREDSYMB_DEF_LEN 350

//PERV_logicSymb
#define LOGICSYMB_DEF \
"PERV_LogicSymbTypes PERV_logicSymb(int index)                                 \n{                                                                              \n    return ((PERV_LogicSymbTypes)(index - PREV_LSSTART));                      \n}\n\n"
#define LOGICSYMB_DEF_LEN 350
    

//pervasive constants relevant info in pervasives.c
static char* constC = NULL;

void genConstC()
{
  char *myText = genConstDataTab();
  constC = (char*)malloc(sizeof(char*)*(CONST_COMMENTS_C_LEN + strlen(myText) +
                         GETCONSTDATA_DEF_LEN + COPYCONSTDATATAB_DEF_LEN +
                         ISLOGICSYMB_DEF_LEN + ISPREDSYMB_DEF_LEN +
                         LOGICSYMB_DEF_LEN + 1));
  strcpy(constC, CONST_COMMENTS_C);
  strcat(constC, myText);
  strcat(constC, GETCONSTDATA_DEF);
  strcat(constC, COPYCONSTDATATAB_DEF);
  strcat(constC, ISLOGICSYMB_DEF);
  strcat(constC, ISPREDSYMB_DEF);
  strcat(constC, LOGICSYMB_DEF);
  free(myText);
}


//dump peravsives.h
void spitCPervasivesC()
{
    FILE* outFile = fopen("../tables/pervasives.c", "w");
    fprintf(outFile, "%s\n%s\n", COMPDEF_BEG_C, INCLUDE_C);
    fprintf(outFile, "%s\n", kindC);
    fprintf(outFile, "%s\n", tySkelC);
    fprintf(outFile, "%s\n", constC);
    fprintf(outFile, COMPDEF_END_C);
    fclose(outFile);
    free(kindC); free(tySkelC); free(constC);
}


void kindInfoInit(int length)
{
    kindIndices.length = length;
    kindIndices.array  = (char**)malloc(sizeof(char*)*length);
    kindData.length    = length;
    kindData.array     = (char**)malloc(sizeof(char*)*length);
}

void tySkelInfoInit(int length)
{
    if (length == 0) {
        fprintf(stderr, "The number of type skeletons cannot be 0\n");
        exit(1);
    }    
    tySkels.length = length;
    tySkels.array  = (char**)malloc(sizeof(char*)*length);
}

void constInfoInit(int length)
{
    if (length == 0) {
        fprintf(stderr, "The number of constants cannot be 0\n");
        exit(1);
    }
    constIndices.length = length;
    constIndices.array  = (char**)malloc(sizeof(char*)*length);
    constData.length    = length;
    constData.array     = (char**)malloc(sizeof(char*)*length);
}

void lsInfoInit(int length)
{
    if (length == 0) {
        fprintf(stderr, "The number of logic symbols cannot be 0\n");
        exit(1);
    }
    logicSymbTypes.length = length;
    logicSymbTypes.array  = (char**)malloc(sizeof(char*)*length);
}

