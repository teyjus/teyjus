/******************************************************************************/
/* File instrgen-c.h. This files contains function declarations for generating*/
/* files instructions.h and instructions.c                                    */
/******************************************************************************/

/* instructions.h */
/* generating operand types */

void cgenOpTypes(char *name, char* typeName, char* types, char* comments, 
                int last);

void cgenOpCodeType(char* optype);

void cgenOpsH();

void cgenInstrCatH(char* callI1Len);

void cgenOneInstrCatH(char* name, int last);

void cgenInstrLength(char* name, char* length);

void cgenInstrH(char* numInstr);

void cgenOneInstrH(char* comments, char* opcode, char* name);


/* dump instructions.h" */
void cspitCInstructionsH(char * root);

/* instructions.c */

void cgenInstrFormat(char* opType, int last);

void cgenOneInstrCatC(char* name, int last);

void cgenInstrCatC(char* max_op);

void cinitInstrC(int numInstr);
void cgenOneInstrC(int opcode, char* name, char* cat, char* len, int last);
void cgenInstrC();

/* dump instructions.c" */
void cspitCInstructionsC(char * root);

/* simdispatch.c        */
void cinitSimDispatch(int size);
void cgenOneSimDispatch(int ind, char* instr, int last);
void cgenSimDispatch();

void cspitSimDispatch(char * root);
