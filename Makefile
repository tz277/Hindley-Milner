build:
	ocamlbuild lexer.ml parser.ml main.byte type.ml
	

clean:
	ocamlbuild -clean