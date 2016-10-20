all: 
	@ocamlbuild -use-menhir -use-ocamlfind -r src/exp_test.native;
	@printf "\n"
	@echo "Chose one of these options:"
	@echo " - own_test: to test your own test file"
	@echo " - all_tests: to test all the files provided in the test folder"
	@echo " - clean: to clean the project of any junk files"

own_test:  
	@echo "Enter the path to a file, containing the function(.txt only):"; \
	read path1; \
	echo "Enter the path to a file, containing the expected result (.txt only): "; \
	read path2; \
	sh individual_test.sh $$path1 $$path2

all_tests:
	@sh test.sh part1;
	@sh test.sh part2;

clean:
	@echo "Removing build files.";
	@rm -f exp_test.native;
	@rm -rf _build/;