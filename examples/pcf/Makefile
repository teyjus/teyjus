# Modify the path to the Teyjus executables as needed
TEYJUS= ../../source
TJCC= $(TEYJUS)/tjcc
TJLINK= $(TEYJUS)/tjlink

all: eval monotyping polytyping tailrecursion
	

examples.lpo: examples.sig examples.mod pcf.sig
	$(TJCC) examples

control.lpo: control.mod control.sig
	$(TJCC) control

eval: eval_test.lpo eval.lpo examples.lpo
	$(TJLINK) eval_test

eval_test.lpo: eval_test.mod eval_test.sig eval.sig examples.sig pcf.sig
	$(TJCC) eval_test

eval.lpo: eval.mod eval.sig control.sig pcf.sig
	$(TJCC) eval

monotyping: mono_test.lpo monoinfer.lpo examples.lpo
	$(TJLINK) mono_test

mono_test.lpo: mono_test.mod mono_test.sig monoinfer.sig pcf.sig monotypes.sig exampes.sig
	$(TJCC) mono_test

monoinfer.lpo: monoinfer.mod monoinfer.sig pcf.sig monotypes.sig
	$(TJCC) monoinfer

polytyping: poly_test.lpo polyinfer.lpo examples.lpo unifytypes.lpo control.lpo 
	$(TJLINK) poly_test

unifytypes.lpo: unifytypes.mod unifytypes.sig monotypes.sig control.sig
	$(TJCC) unifytypes

polyinfer.lpo: polyinfer.mod polyinfer.sig polytypes.sig monotypes.sig pcf.sig unifytypes.sig
	$(TJCC) polyinfer

poly_test.lpo: poly_test.mod poly_test.sig polyinfer.sig pcf.sig polytypes.sig monotypes.sig examples.sig 
	$(TJCC) poly_test

tailrecursion: tr_test.lpo tailrec.lpo reflsyntax.lpo
	$(TJLINK) tr_test

tr_test.lpo: tr_test.sig tr_test.mod tailrec.sig pcf.sig examples.sig
	$(TJCC) tr_test.mod

tailrec.lpo: tailrec.mod tailrec.sig pcf.sig refl_syntax.sig
	$(TJCC) tailrec

refl_syntax.lpo: refl_syntax.mod refl_syntax.sig pcf.sig
	$(TJCC) refl_syntax

clean:
	rm -f *.lpo *.lp
