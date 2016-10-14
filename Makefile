all: 
	@ocamlbuild -use-menhir -use-ocamlfind -r src/exp_test.native;
	printf "\n"
	@echo "Chose one of these options:"
	@echo " - own_test: to test your own test file"
	@echo " - all_tests: to test all the files provided in the test folder"
	@echo " - clean: to clean the project of any junk files"

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