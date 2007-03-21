AM_CPPFLAGS = -I${top_srcdir}/source

bin_PROGRAMS = linkcheck

TSTSRCS = TestFile.c TestKind.c TestTySkel.c

linkcheck_SOURCES =$(TSTSRCS) cutcheck.c
linkcheck_LDADD = ../liblink.a

TESTS = linkcheck

CLEANFILES = cutcheck.c

cutcheck.c: $(TSTSRCS)
	cutgen-2 $(TSTSRCS) -o cutcheck.c
