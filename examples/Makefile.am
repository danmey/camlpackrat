bin_SCRIPTS = calc$(EXEEXT)
PACKAGES = 
MLPEG_LIB = ../mlpeg.cma
LIBS     = $(MLPEG_LIB)

calc$(EXEEXT): calcparser.cmo calc.cmo 
	$(OCAMLFIND) ocamlc $(PACKAGES) $(LIBS) $^ -o $@

calcparser.cmo: calcparser.mlpeg
	../mlpeg $< > calcparser.ml 
	ocamlfind ocamlc $(PACKAGES) $(LIBS) -I ../src -c calcparser.ml  -o $@

.ml.cmo:
	ocamlfind ocamlc $(PACKAGES) -I ../src -c $< -o $@


EXTRA_DIST = calc.mlpeg calcparser.ml
CLEANFILES = *.cmi *.cmo $(bin_SCRIPTS) *~
