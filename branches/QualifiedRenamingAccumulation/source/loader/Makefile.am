noinst_LIBRARIES = libloader.a

SIMSRCS = ../simulator/mcstring.c ../simulator/dataformats.c      ../simulator/io-datastructures.c

SYSSRCS = ../system/error.c  ../system/memory.c  ../system/message.c  ../system/stream.c

TABSRCS = ../tables/instructions.c  ../tables/pervasives.c

LDSRCS = loader.c strings.c file.c kind.c const.c hashtab.c code.c bvrtab.c implgoal.c searchtab.c importtab.c tyskel.c

libloader_a_SOURCES = $(LDSRCS) $(SIMSRCS) $(SYSSRCS) $(TABSRCS)

SUBDIRS = . test