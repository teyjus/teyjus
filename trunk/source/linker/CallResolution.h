#ifndef _CALLRESOLUTION_H_
#define _CALLRESOLUTION_H_

#include "datatypes.h"
#include "vector.h"
#include "hashtab.h"

typedef struct Vector PredInfoTab;

/**
\brief Initialize a Predicate information table for use.
\arg Pit a pointer to the table to initialize
**/
extern void InitInfoTab(PredInfoTab* Pit);

/**
\brief Report an occurance of a call or execute instruction.
\arg Pit The predicate info table to report to.
\arg index The constant index of the predicate being called.
\arg addr The address of the call instruction.
\arg exec_flag A flag indicating whether the instruction is execute or call.
**/
extern void PushCall(PredInfoTab* Pit, ConstInd index,CodeInd addr,int exec_flag);

/**
\brief That a predicate may be extended at runtime.
\arg Pit The predicate info table to report to.
\arg index The constant index of the predicate which may be extended.
**/
extern void MarkDynamic(PredInfoTab* Pit, ConstInd index);

/**
\brief Resolve all of the predicate calls reported to a predicate info table.
\arg Pit The predicate info table.
\arg PredSearchTab The search table to use to get code addresses.
**/
extern void ResolvePredCalls(PredInfoTab* Pit, HashTab_t* PredSearchTab);

#endif
