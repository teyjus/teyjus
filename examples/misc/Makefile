# Modify the path to the Teyjus executables as needed
TEYJUS= /home/gopalan/projects/teyjus/source
TJCC= $(TEYJUS)/tjcc

all: lists lists2 assoc reverse funs maps
	

lists: lists.lpo
	

lists.lpo: lists.sig lists.mod
	$(TJCC) lists


lists2: lists2.lpo
	

lists2.lpo: lists.sig lists.mod
	$(TJCC) lists


assoc: assoc.lpo assoclists.lpo
	

assoc.lpo: assoc.sig assoc.mod
	$(TJCC) assoc

assoclists.lpo: assoclists.sig assoclists.mod
	$(TJCC) assoclists


reverse: reverse.lpo
	

reverse.lpo: reverse.sig reverse.mod
	$(TJCC) reverse


funs: funs.lpo
	

funs.lpo: funs.sig funs.mod
	$(TJCC) funs


maps: maps.lpo
	

maps.lpo: maps.sig maps.mod
	$(TJCC) maps


clean:
	rm -f *.lpo *.lp
