AM_CPPFLAGS = -I${top_srcdir}/source

bin_PROGRAMS = loadcheck

TSTSRCS = TestFile.c TestString.c TestKind.c TestConst.c TestHashtab.c TestBvrtab.c TestTySkel.c

loadcheck_SOURCES =$(TSTSRCS) cutcheck.c
loadcheck_LDADD = ../libloader.a

TESTS = loadcheck

CLEANFILES = cutcheck.c

cutcheck.c: $(TSTSRCS)
	cutgen-2 $(TSTSRCS) -o cutcheck.c
