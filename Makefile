all: 
	@ocamlbuild -use-menhir -use-ocamlfind -r src/exp_test.native;

own_test:  
	@echo "Enter path to test file (.txt only); E.g. test/test1.txt:"; \
	read path; \
	sh individual_test.sh $$path

all_tests:
	@sh test.sh;

clean:
	@echo "Removing build files.";
	@rm -f exp_test.native;
	@rm -rf _build/;