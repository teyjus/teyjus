#ifndef _CODE_H_
#define _CODE_H_

#include "datatypes.h"

void InitTCode();
void LoadCode();
void WriteCode();
void LoadCodeSize();

CodeInd MergeSubSequence(CodeInd a, CodeInd b,INT1 n);
CodeInd MergeDefs(CodeInd a, CodeInd b);

void MakeCallName(CodeInd from, int exec_flag, ConstInd to);
void MakeCall(CodeInd from, int exec_flag, CodeInd to);

#endif
