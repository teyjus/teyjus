tjdis:
	ocamake disassemblerfront.ml parseargs.ml parseargs.mli ../compiler/bytecode.ml ../compiler/bytecode.mli ../compiler/absyn.ml ../compiler/absyn.mli ../compiler/table.ml ../compiler/table.mli ../compiler/symbol.ml ../compiler/symbol.mli ../compiler/option.ml ../compiler/option.mli ../compiler/errormsg.ml ../compiler/errormsg.mli ../compiler/pervasive.ml ../compiler/pervasive.mli ../compiler/pervasiveutils.ml ../compiler/pervasiveutils.mli ../compiler/instr.ml ../compiler/instr.mli ../disassembler/context.ml ../disassembler/context.mli ../disassembler/label.ml ../disassembler/label.mli ../disassembler/disassembly.ml ../disassembler/disassembly.mli -opt -o tjdis

tjcc:
	ocamake compilerfront.ml parseargs.ml parseargs.mli ../compiler/*.ml ../compiler/*.mli ../compiler/*.mll ../compiler/*.mly -opt -o tjcc


tjsim:
	ocamake parseargs.ml parseargs.mli front.mli simulatorfront.ml ../compiler/*.ml ../compiler/*.mli -lp front_stubs.o front.o test.o ../tables/*.o ../system/*.o ../simulator/*.o ../simulator/builtins/*.o -opt -o tjsim

clean:
	rm *.cmx ../compiler/*.cmx ../disassembler/*.cmx