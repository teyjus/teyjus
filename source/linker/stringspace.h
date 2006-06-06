#ifndef _STRINGSPACE_H_
#define _STRINGSPACE_H_

//StringSpace Header Data
//Note: Uses CM->StringSpacecount && CM->StringSpaceoffset

void InitTStringSpaces();
void LoadStringSpaces();
void WriteStringSpaces();
void LoadStringSpaceSize();

typedef struct{
	int size;
	char* string;
}Name;

Name GetName();

#endif