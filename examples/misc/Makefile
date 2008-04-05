# Modify the path to the Teyjus executables as needed
TEYJUS= ../../source
TJCC= $(TEYJUS)/tjcc
TJLINK= $(TEYJUS)/tjlink

all: lists lists2 assoc reverse funs maps
		

lists: lists.lp
	

lists.lp: lists.lpo
	$(TJLINK) lists

lists.lpo: lists.sig lists.mod
	$(TJCC) lists


lists2: lists2.lp
	

lists2.lp: lists2.lpo
	$(TJLINK) lists2

lists2.lpo: lists2.sig lists2.mod
	$(TJCC) lists2


assoc: assoc.lp
	

assoc.lp: assoc.lpo assoclists.lpo
	$(TJLINK) assoc

assoc.lpo: assoc.sig assoc.mod
	$(TJCC) assoc

assoclists.lpo: assoclists.sig assoclists.mod
	$(TJCC) assoclists


reverse: reverse.lp
	

reverse.lp: reverse.lpo
	$(TJLINK) reverse

reverse.lpo: reverse.sig reverse.mod
	$(TJCC) reverse


funs: funs.lp
	

funs.lp: funs.lpo
	$(TJLINK) funs

funs.lpo: funs.sig funs.mod
	$(TJCC) funs


maps: maps.lp
	

maps.lp: maps.lpo
	$(TJLINK) maps

maps.lpo: maps.sig maps.mod
	$(TJCC) maps


clean:
	rm -f *.lpo *.lp
