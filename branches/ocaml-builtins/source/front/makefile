tjsim:
	ocamake *.ml *.mli ../compiler/*.ml ../compiler/*.mli ../loader/*.ml ../loader/*.mli -lp ccode_stubs.o front_c.o query_c.o ../tables/*.o ../system/*.o ../simulator/*.o ../simulator/builtins/*.o -opt -o tjsim


clean:
	rm *.cmx ../compiler/*.cmx ../disassembler/*.cmx ../myloader/*.cmx

