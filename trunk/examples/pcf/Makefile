# Modify the path to the Teyjus executables as needed
TEYJUS= ../../source
TJCC= $(TEYJUS)/tjcc
TJLINK= $(TEYJUS)/tjlink

.PHONY: all eval monotyping polytyping tailrecursion
all: eval monotyping polytyping tailrecursion

eval: eval_test.lp
monotyping: mono_test.lp
polytyping: poly_test.lp
tailrecursion: tr_test.lp

# Implicit rules for compiling teyjus code
# The variable $* refers to name matched by %

%.lpo : %.mod %.sig
	$(TJCC) $*

%.lp : %.lpo
	$(TJLINK) $*


# Dependencies must be listed explicitly

eval_test.lp: eval_test.lpo eval.lpo examples.lpo control.lpo

eval_test.lpo: eval.sig examples.sig pcf.sig
eval.lpo: control.sig pcf.sig
examples.lpo: pcf.sig
control.lpo:


mono_test.lp: mono_test.lpo monoinfer.lpo examples.lpo

mono_test.lpo: monoinfer.sig pcf.sig monotypes.sig examples.sig
monoinfer.lpo: pcf.sig monotypes.sig


poly_test.lp: poly_test.lpo polyinfer.lpo examples.lpo unifytypes.lpo control.lpo

unifytypes.lpo: monotypes.sig control.sig
polyinfer.lpo: polytypes.sig monotypes.sig pcf.sig unifytypes.sig
poly_test.lpo: polyinfer.sig pcf.sig polytypes.sig monotypes.sig examples.sig

tr_test.lp: tr_test.lpo tailrec.lpo refl_syntax.lpo examples.lpo

tr_test.lpo: tailrec.sig pcf.sig examples.sig
tailrec.lpo: pcf.sig refl_syntax.sig
refl_syntax.lpo: pcf.sig


# Utilities

clean:
	rm -f *.lpo *.lp
