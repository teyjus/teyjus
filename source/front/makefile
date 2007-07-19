tjsim:
	ocamake parseargs.ml parseargs.mli simulatorfront.ml module.ml module.mli query.ml query.mli front.ml front.mli readterm.ml readterm.mli ccode_stubs.mli simerrors.mli simerrors.ml ../compiler/*.ml ../compiler/*.mli ../loader/*.ml ../loader/*.mli -lp ccode_stubs.o front_c.o query_c.o ../tables/*.o ../system/*.o ../simulator/*.o ../simulator/builtins/*.o -opt -o tjsim


clean:
	rm *.cmx ../compiler/*.cmx ../disassembler/*.cmx ../myloader/*.cmx