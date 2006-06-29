#ifndef _STRINGSPACE_H_
#define _STRINGSPACE_H_

////////////////////////////////////////////////////////////////////
//Provides functions used in loading the string space of a module.//
////////////////////////////////////////////////////////////////////

//StringSpace Header Data
//Note: Uses CM->StringSpacecount && CM->StringSpaceoffset

extern void InitTStringSpaces();
extern void LoadStringSpaces();
extern void WriteStringSpaces();

#endif
