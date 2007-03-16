bin_PROGRAMS = loadcheck

TSTSRCS = TestFile.c TestString.c TestKind.c TestConst.c TestThrow.c TestHashtab.c TestBvrtab.c TestTySkel.c

loadcheck_SOURCES =$(TSTSRCS) cutcheck.c
loadcheck_LDADD = ../libloader.a

TESTS = loadcheck

cutcheck.c: $(TSTSRCS)
	cutgen-2 $(TSTSRCS) -o cutcheck.c
