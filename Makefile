	ifdef	CROSS
OCAMLOPT=i686-w64-mingw32-ocamlopt
OCAMLC=i686-w64-mingw32-ocamlc
OCAMLDEP=i686-w64-mingw32-ocamldep
	else
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
OCAMLDEP=ocamldep
	endif

INCL=

#OCAMLNLDFLAGS = -ccopt -static
OCAMLFLAGS=-unsafe -bin-annot -warn-error +a -w +a-42-45 -safe-string

VERSION=0.0.1

SRCMLI=fileExt.mli stringExt.mli listExt.mli hashtblExt.mli bytesExt.mli emit.mli
SRCMLI+=log.mli
SRCMLI+=archive.mli
SRCMLI+=aout.mli
SRCMLI+=problem.mli
SRCMLI+=linker.mli
SRCMLI+=alcyon.mli
SRCMLI+=coff.mli

SRCML=fileExt.ml stringExt.ml listExt.ml hashtblExt.ml bytesExt.ml emit.ml
SRCML+=version.ml
SRCML+=log.ml
SRCML+=archive.ml
SRCML+=aout.ml
SRCML+=problem.ml
SRCML+=linker.ml
SRCML+=alcyon.ml
SRCML+=coff.ml
SRCML+=main.ml

SRCS=$(SRCML) $(SRCMLI)

PROJECT=jlinker

EXTRA=README.md Makefile

LIBS=

BYTELIBS=$(LIBS:=.cma)
NATIVELIBS=$(LIBS:=.cmxa)

CMI=$(SRCMLI:.mli=.cmi)
CMO=$(SRCML:.ml=.cmo)
CMX=$(SRCML:.ml=.cmx)

all: .depend $(PROJECT).native $(PROJECT).byte

.PHONY: all clean dist

$(PROJECT).native: $(CMX)
	$(OCAMLOPT) $(INCL) -o $@ $(NATIVELIBS) $^

$(PROJECT).byte: $(CMO)
	$(OCAMLC) $(INCL) -o $@ $(BYTELIBS) $^

version.ml: Makefile
	@echo "let date_of_compile=\""`date`"\";;" > $@
	@echo "let version=\""$(VERSION)"\";;" >> $@
	@echo "let build_info=\""`uname -msrn`"\";;" >> $@
	@echo "let revision=\""`git log -1 --format="%h"`"\";;" >> $@

dist: $(SRCS) $(EXTRA)
	mkdir $(PROJECT)
	cp $(SRCS) $(EXTRA) $(PROJECT)
	tar cfvz $(PROJECT)-$(VERSION).tar.gz $(PROJECT)
	rm -rf $(PROJECT)

%.cmo: %.ml
	$(OCAMLC) $(INCL) -c $(OCAMLFLAGS) -o $@ $<

%.cmi: %.mli
	$(OCAMLC) $(INCL) -c $(OCAMLFLAGS) -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(INCL) -c $(OCAMLFLAGS) -o $@ $<

clean:
	rm -f $(CMI) $(CMO) $(CMX) $(SRCML:.ml=.o) $(SRCML:.ml=.annot) $(SRCML:.ml=.cmi) $(SRCML:.ml=.cmt) $(SRCML:.ml=.cmti) version.ml $(PROJECT).byte $(PROJECT).native *~

.depend: $(SRCS)
	$(OCAMLDEP) $(INCL) $(SRCS) > .depend

-include .depend
