#ifndef _CODE_H_
#define _CODE_H_

#include "datatypes.h"

extern void InitTCode();
extern void LoadCode();
extern void WriteCode();
extern void WriteCodeSize();
extern void LoadCodeSize();

extern CodeInd MergeSubSequence(CodeInd a, CodeInd b,INT1 n);
extern CodeInd MergeDefs(CodeInd a, CodeInd b);

extern void MakeCallName(CodeInd from, int exec_flag, ConstInd to);
extern void MakeCall(CodeInd from, int exec_flag, CodeInd to);

#endif
