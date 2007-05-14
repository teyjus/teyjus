##AM_CFLAGS= -Wall -Wno-comment
AM_CFLAGS = -I${top_srcdir}/source

bin_PROGRAMS = linker

noinst_LIBRARIES = liblink.a

SIMSRCS = ../simulator/mcstring.c ../simulator/dataformats.c      ../simulator/io-datastructures.c

SYSSRCS = ../system/error.c  ../system/memory.c  ../system/message.c  ../system/stream.c

TABSRCS = ../tables/instructions.c  ../tables/pervasives.c

LKSRCS = CallResolution.c bvrtab.c const.c hashtab.c importtab.c rename.c tyskel.c code.c file.c implgoal.c kind.c module.c stringspace.c tree.c vector.c VectorRW.c

liblink_a_SOURCES = $(LKSRCS) $(TABSRCS) $(SIMSRCS) $(SYSSRCS)

linker_SOURCES = main.c
linker_LDADD = liblink.a

SUBDIRS = . test