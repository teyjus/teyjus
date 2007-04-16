#ifndef _BVRTAB_H_
#define _BVRTAB_H_

extern void InitTBvrTabs();
extern void LoadBvrTabs(int fd, struct Module_st* CMData);
extern void WriteBvrTabs(int fd);

extern void MergeBvrTabs(BvrTabInd a, BvrTabInd b,Byte n);

#endif //_BVRTAB_H_
