bin_SCRIPTS = mlpeg$(EXEEXT)
PACKAGES = 
LIBS     = 

mlpeg$(EXEEXT): mstream.cmo helper.cmo mlpegt.cmo parserlang.cmo pegparser.cmo peglexer.cmo mlcodegen.cmo mlpeg.cmo
	$(OCAMLFIND) ocamlc $(PACKAGES) $(LIBS) $^ -o $@

peglexer.ml: peglexer.mll
	ocamllex $< -o $@

pegparser.ml: pegparser.mly
	ocamlyacc $<

.ml.cmo:
	ocamlfind ocamlc $(PACKAGES) -c $< -o $@

EXTRA_DIST = gtk_test.ml

CLEANFILES = *.cmi *.cmo $(bin_SCRIPTS) *~
