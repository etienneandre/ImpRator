###############################################################
#
#                    IMPERATOR
#
#  Blublu
#
#  LSV
#  Author: Etienne Andre
#  Created:       2009/03
#  Last modified: 2009/03/26
#  Ocaml version: 3.09.3
###############################################################

# CONSTANTS
OCAMLIB_PATH = /usr/lib/ocaml

# FILES
.PREFIXES : +.
.SUFFIXES : .cmo .cmi .ml .mli

FILES =  Global.+ Constraint.+ Matrix.+ AbstractImperatorFile.+ ImperatorLexer.+ ImperatorParser.+ ImperatorPrinter.+ IMPERATOR.+
FILESCMI = Global.cmi Constraint.cmi Matrix.cmi AbstractImperatorFile.cmi


all:
	make parser
	make compil
	make rmtpf
	make rmuseless


header:
	ocamlc -c Global.mli
	ocamlc -c Global.ml

	ocamlc -c Constraint.mli
	ocamlc -c Constraint.ml
	
	ocamlc -c Matrix.mli
	ocamlc -c Matrix.ml
	
	ocamlc -c AbstractImperatorFile.mli
	ocamlc -c AbstractImperatorFile.ml


parser:
	make header
	ocamllex ImperatorLexer.mll       # generates ImperatorLexer.ml
	ocamlyacc ImperatorParser.mly     # generates ImperatorParser.ml and ImperatorParser.mli

compil:
	make header

	ocamlc -c ImperatorParser.mli
	ocamlc -c ImperatorLexer.ml
	ocamlc -c ImperatorParser.ml
	ocamlc -c ImperatorPrinter.mli
	ocamlc -c ImperatorPrinter.ml
	
	ocamlc -c IMPERATOR.ml
	ocamlc -I $(OCAMLIB_PATH) nums.cma str.cma unix.cma -o IMPRATOR $(FILES:+=cmo)

exe:
	make compil
	make rmtpf
	make rmuseless
	./IMPRATOR

count:
	make clean
	python lineCounter.py

clean:
	make rmtpf
	make rmuseless
	rm -rf ImperatorLexer.ml ImperatorParser.ml ImperatorParser.mli
	rm -rf IMPRATOR

rmuseless:
	rm -rf $(FILES:+=cmo) $(FILES:+=cmi) $(FILES:+=o)
	rm -rf $(FILESCMI)

rmtpf:
	rm -rf *~
#	rm -rf *.ml~ *.mli~ *.mly~ *.mll~ *.imp~ Makefile~


