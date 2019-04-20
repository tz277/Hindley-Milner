build:
	ocamlbuild lexer.ml parser.ml main.byte
	

clean:
	ocamlbuild -clean