all:
	@ocamlbuild -use-menhir -use-ocamlfind -r -pkg unix src/exp_test.native;
	@printf "\n"
	@echo "Chose one of these options:"
	@echo " - parse: to parse"
	@echo " - evaluate: to evaluate"
	@echo " - optimise: to optimise"
	@echo " - interpret: to interpret only"
	@echo " - generate: to generate assembly code only"
	@echo " - clean: to clean the project of any junk files"

parse:
	@sh test.sh -p;

evaluate:
	@sh test.sh -e;

optimise:
	@sh test.sh -o;

interpret:
	@sh test.sh -i;

generate:
	@sh test.sh -g;

clean:
	@echo "Removing build files.";
	@rm -f exp_test.native;
	@rm -rf _build/;
