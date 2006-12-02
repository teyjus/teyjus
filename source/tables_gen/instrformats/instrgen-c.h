/******************************************************************************/
/* File instrgen-c.h. This files contains function declarations for generating*/
/* files instructions.h and instructions.c                                    */
/******************************************************************************/

/* instructions.h */
/* generating operand types */

void genOpTypes(char *name, char* typeName, char* types, char* comments, 
                int last);

void genOpCodeType(char* optype);

void genOpsH();

void genInstrCatH(char* callI1Len);

void genOneInstrCatH(char* name, int last);

void genInstrLength(char* name, char* length);

void genInstrH(char* numInstr);

void genOneInstrH(char* comments, char* opcode, char* name);


/* dump instructions.h" */
void spitCInstructionsH();

/* instructions.c */

void genInstrFormat(char* opType, int last);

void genOneInstrCatC(char* name, int last);

void genInstrCatC(char* max_op);

void initInstrC(int numInstr);
void genOneInstrC(int opcode, char* name, char* cat, char* len, int last);
void genInstrC();

/* dump instructions.c" */
void spitCInstructionsC();

/* simdispatch.c        */
void initSimDispatch(int size);
void genOneSimDispatch(int ind, char* instr, int last);
void genSimDispatch();

void spitSimDispatch();
