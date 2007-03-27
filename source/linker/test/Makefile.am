AM_CPPFLAGS = -I${top_srcdir}/source -g

bin_PROGRAMS = linkcheck

TSTSRCS =TestInits.c TestFile.c TestKind.c TestTySkel.c TestConst.c TestModule.c TestString.c

linkcheck_SOURCES =$(TSTSRCS) cutcheck.c
linkcheck_LDADD = ../liblink.a

TESTS = linkcheck

CLEANFILES = cutcheck.c

cutcheck.c: $(TSTSRCS)
	cutgen-2 $(TSTSRCS) -o cutcheck.c
