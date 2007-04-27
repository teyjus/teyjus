/*************************************************************************/
/* functions for generating ocaml instr.mli and instr.ml                 */
/*************************************************************************/
/* include */
void ocgenInclude(char* include);

/* operand types */
void ocgenOpType(char* typeName, int numBytes, char* compType);
void ocgenOpCodeType(int numBytes);
void ocgenOps();

/* instruction category */
void ocgenInstrFormat(char* opName);
void ocgenOneInstrCat(char* catName);
void ocgenInstrLength(char* varName, char* numBytes);
void ocgenInstrCat();

/* instructions */
void ocgenOneInstr(char* opcode, char* insName, char* insCat, char* insLength);
void ocgenInstr();


/* dump files */
void ocSpitInstructionMLI();
void ocSpitInstructionML();

