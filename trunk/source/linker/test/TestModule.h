#ifndef TEST_MODULE_H_
#define TEST_MODULE_H_

extern void TEST_CreateM1CodeSize(int fd);

extern void TEST_CreateM1GKindTable(int fd);
extern void TEST_CreateM1LKindTable(int fd);

extern void TEST_CheckM1GKindTable(int fd);
extern void TEST_CheckM1LKindTable(int fd);
extern void TEST_CheckM1KindTables(int fd);

extern void TEST_CreateM1TySkelTable(int fd);
extern void TEST_CheckM1TySkelTable(int fd);

extern void TEST_CreateM1GConstTable(int fd);
extern void TEST_CreateM1LConstTable(int fd);
extern void TEST_CreateM1HConstTable(int fd);
extern void TEST_CreateM1ConstTables(int fd);

extern void TEST_CheckM1GConstTable(int fd);
extern void TEST_CheckM1LConstTable(int fd);
extern void TEST_CheckM1HConstTable(int fd);
extern void TEST_CheckM1ConstTables(int fd);

extern void TEST_CreateM1StringTable(int fd);
extern void TEST_CheckM1StringTable(int fd);

extern void TEST_CreateM1ImplGoalTable(int fd);
extern void TEST_CheckM1ImplGoalTable(int fd);

extern void TEST_CreateM1HashTabs(int fd);
extern void TEST_CheckM1HashTabs(int fd);

extern void TEST_CreateM1BvrTabs(int fd);
extern void TEST_CheckM1BvrTabs(int fd);

extern void TEST_CreateM1ImportTable(int fd);
extern void TEST_CheckM1ImportTable(int fd);

extern void TEST_CreateM1Code(int fd);
extern void TEST_CheckM1Code(int fd);

#endif
