#ifndef TEST_MODULE_H_
#define TEST_MODULE_H_

extern void TEST_CreateM1M2KindRenameTable(int fd);

extern void TEST_CreateM2GKindTable(int fd);
extern void TEST_CreateM2LKindTable(int fd);
extern void TEST_CreateM2KindTables(int fd);

extern void TEST_CheckM2GKindTable(int fd);
extern void TEST_CheckM2LKindTable(int fd);

#endif
