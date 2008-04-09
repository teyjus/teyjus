# Modify the path to the Teyjus executables as needed
TEYJUS= ../../../source
TJCC= $(TEYJUS)/tjcc
TJLINK= $(TEYJUS)/tjlink

.PHONY: hcinterp hcsyntax pnf
all: hcinterp hcsyntax pnf
hcinterp: hcinterp_examples.lp
hcsyntax: hcsyntax_examples.lp
pnf: pnf_examples.lp


# Implicit rules for compiling teyjus code
# The variable $* refers to name matched by %

%.lpo : %.mod %.sig
	$(TJCC) $*

%.lp : %.lpo
	$(TJLINK) $*


# Dependencies must be listed explicitly

hcinterp_examples.lp: hcinterp_examples.lpo hc_interp.lpo lists.lpo
hcsyntax_examples.lp: hcsyntax_examples.lpo hc_syntax.lpo refl_syntax.lpo
pnf_examples.lp: pnf_examples.lpo refl_syntax.lpo pnf.lpo 

hcinterp_examples.lpo: logic_basic.sig logic_types.sig logic_vocab.sig
hcsyntax_examples.lpo: hc_syntax.sig refl_syntax.sig 
hcsyntax_examples.lpo: logic_types.sig logic_basic.sig logic_vocab.sig
pnf_examples.lpo: refl_syntax.sig pnf.sig logic_types.sig 
pnf_examples.lpo: logic_basic.sig logic_vocab.sig 
hc_interp.lpo: lists.sig logic_types.sig logic_basic.sig logic_vocab.sig
lists.lpo:
hc_syntax.lpo: logic_basic.sig logic_types.sig
refl_syntax.lpo: logic_types.sig logic_basic.sig logic_vocab.sig
pnf.lpo: logic_types.sig logic_basic.sig

# Utilities

clean:
	rm -f *.lpo *.lp
