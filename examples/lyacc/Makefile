lpparser.mod: *.mod *.sig
	sh lyacc lpgram lpparser
	reset # fix terminal after errors

.PHONY: clean
clean:
	rm -f *.lp *.lpo lpparser.*
