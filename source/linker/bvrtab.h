#ifndef _BVRTAB_H_
#define _BVRTAB_H_

//BvrTab Header Data
//Note: Uses CM->BvrTaboffset & CM->BvrTabcount


void InitTBvrTabs();
void LoadBvrTabs();
void WriteBvrTabs();

void MergeBvrTabs(BvrTabInd a, BvrTabInd b,INT1 n);

#endif //_BVRTAB_H_
