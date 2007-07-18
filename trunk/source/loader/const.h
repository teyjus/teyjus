#ifndef _CONST_H_
#define _CONST_H_

//!Load the const table of a file.  Sets the global const counter.
extern int LD_CONST_LoadCst(MEM_GmtEnt* ent);

//!Read an index in multi-table form and return it in single table form.  Relies on global const counter.
extern TwoBytes LD_CONST_GetConstInd();

#endif //_KIND_H_
