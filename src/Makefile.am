bin_SCRIPTS = mlpeg$(EXEEXT) mlpeg.cma
PACKAGES = 
LIBS     = 
SRC_OBJ = helper.cmo mstream.cmo mlpegt.cmo parserlang.cmo pegparser.cmo peglexer.cmo mlcodegen.cmo mlpeg.cmo

mlpeg$(EXEEXT): $(SRC_OBJ) mlpeg.cma
	$(OCAMLFIND) ocamlc $(PACKAGES) $(LIBS) $^ -o $@

mlpeg.cma: $(SRC_OBJ)
	$(OCAMLFIND) ocamlc $(PACKAGES) $(LIBS) -a $^ -o $@
	cp mlpeg.cma ..

peglexer.cmo: peglexer.mll
	ocamllex $< -o peglexer.ml
	$(OCAMLFIND) ocamlc $(PACKAGES) -c peglexer.ml -o $@

pegparser.cmo: pegparser.mly
	ocamlyacc $<
	rm -f pegparser.mli
	$(OCAMLFIND) ocamlc $(PACKAGES) -c helper.ml pegparser.ml -o $@

.ml.cmo:
	ocamlfind ocamlc $(PACKAGES) -c $< -o $@

EXTRA_DIST = mstream.ml helper.ml mlpegt.ml parserlang.ml pegparser.mly peglexer.mll mlcodegen.ml mlpeg.ml
CLEANFILES = *.cmi *.cmo $(bin_SCRIPTS) *~
