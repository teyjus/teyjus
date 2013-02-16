##AM_CFLAGS= -Wall -Wno-comment
AM_CFLAGS = -I${top_srcdir}/source

bin_PROGRAMS = linkcheck

TSTUTIL = TestModule.c TestM2.c

##TSTSRCS = TestTree.c TestInits.c TestFile.c TestKind.c TestTySkel.c TestConst.c TestString.c TestHashTabs.c TestImplGoal.c TestBvrTabs.c TestImportTab.c

TSTSRCS = TestInits.c TestCode.c

linkcheck_SOURCES =$(TSTSRCS) $(TSTUTIL) cutcheck.c
linkcheck_LDADD = ${top_srcdir}/source/linker/liblink.a

TESTS = linkcheck

CLEANFILES = cutcheck.c

cutcheck.c: $(TSTSRCS)
	cutgen-2 $(TSTSRCS) -o cutcheck.c
