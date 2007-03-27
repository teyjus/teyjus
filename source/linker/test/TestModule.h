#ifndef TEST_MODULE_H_
#define TEST_MODULE_H_

extern void TEST_CreateM1GKindTable(int fd);

extern void TEST_CheckGKindTable(int fd);

extern void TEST_CreateM1LKindTable(int fd);

extern void TEST_CheckLKindTable(int fd);

extern void TEST_CreateM1M2KindRenameTable(int fd);

extern void TEST_CreateM2KindTable(int fd);

extern void TEST_CheckRNKindTable(int fd);

extern void TEST_CreateM1TySkelTable(int fd);

extern void TEST_CheckTySkelTable(int fd);

extern void TEST_CreateM1GConstTable(int fd);

extern void TEST_CheckM1GConstTable(int fd);

extern void TEST_CreateM1LConstTable(int fd);

extern void TEST_CheckM1LConstTable(int fd);

#endif
