# Modify the path to the Teyjus executables as needed
TEYJUS= ../../source
TJCC= $(TEYJUS)/tjcc
TJLINK= $(TEYJUS)/tjlink

inter: inter.lp
	

inter.lp: inter.lpo ndtac.lpo tacticals.lpo formulas.lpo goalred.lpo listmanip.lpo
	$(TJLINK) inter

inter.lpo: inter.mod inter.sig logic.sig nonlogical.sig formulas.sig ndtac.sig ndproofs.sig goaltypes.sig tacticals.sig
	$(TJCC) inter

ndtac.lpo: ndtac.mod ndtac.sig goaltypes.sig ndproofs.sig logic.sig listmanip.sig
	$(TJCC) ndtac

tacticals.lpo: tacticals.mod tacticals.sig goalred.sig goaltypes.sig
	$(TJCC) tacticals

formulas.lpo: formulas.mod formulas.sig logic.sig nonlogical.sig
	$(TJCC) formulas

goalred.lpo: goalred.mod goalred.sig goaltypes.sig
	$(TJCC) goalred

listmanip.lpo: listmanip.mod listmanip.sig 
	$(TJCC) listmanip

clean:
	rm -f *.lpo *.lp
