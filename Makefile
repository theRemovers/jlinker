OCAMLOPT=ocamlopt
OCAMLC=ocamlc
OCAMLDEP=ocamldep

INCL=

#OCAMLNLDFLAGS = -ccopt -static
OCAMLFLAGS=-unsafe -annot -warn-error +a -w +a-42-45

VERSION=0.0.1

SRCMLI=fileExt.mli stringExt.mli listExt.mli hashtblExt.mli bytes.mli emit.mli
SRCMLI+=archive.mli 
SRCMLI+=aout.mli 
SRCMLI+=log.mli
SRCMLI+=problem.mli
SRCMLI+=linker.mli

SRCML=fileExt.ml stringExt.ml listExt.ml hashtblExt.ml bytes.ml emit.ml
SRCML+=version.ml
SRCML+=archive.ml
SRCML+=aout.ml
SRCML+=log.ml
SRCML+=problem.ml
SRCML+=linker.ml
SRCML+=main.ml

SRCS=$(SRCML) $(SRCMLI)

PROJECT=linker

EXTRA=README Makefile

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
	rm -f $(CMI) $(CMO) $(CMX) version.ml

.depend: $(SRCS)
	$(OCAMLDEP) $(INCL) $(SRCS) > .depend

-include .depend

