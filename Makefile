all: 
	@ocamlbuild -use-menhir -use-ocamlfind -r src/exp_test.native;

own_test:  
	@echo "Enter path to test file (.txt only); E.g. tests/test1.txt:"; \
	read path; \
	cat $$path | ./exp_test.native

all_tests:
	@echo  "Test1:";
	@printf "\n";
	@cat tests/test1.txt;
	@echo "Result:";
	@printf "\n";
	@cat tests/test1.txt | ./exp_test.native;
	@printf "\n";
	@echo  "Test2:";
	@printf "\n";
	@cat tests/test2.txt;
	@echo "Result:";
	@printf "\n";
	@cat tests/test2.txt | ./exp_test.native;
	@printf "\n";
	@echo "Test3:";
	@printf "\n";
	@cat tests/test3.txt;
	@echo "Result:";
	@printf "\n";
	@cat tests/test3.txt | ./exp_test.native;
	@printf "\n";
	@echo "Test4:";
	@printf "\n";
	@cat tests/test4.txt;
	@echo "Result:";
	@printf "\n";
	@cat tests/test4.txt | ./exp_test.native;
	@printf "\n";
	@echo "Test5:";
	@printf "\n";
	@cat tests/test5.txt;
	@echo "Result:";
	@printf "\n";
	@cat tests/test5.txt | ./exp_test.native;
	@printf "\n";
	@echo "Iterative bisection:";
	@printf "\n";
	@cat tests/bisection_iterative.txt;
	@echo "Result:";
	@printf "\n";
	@cat tests/bisection_iterative.txt | ./exp_test.native;
	@printf "\n";
	@echo "Recursive bisection:";
	@printf "\n";
	@cat tests/bisection_recursive.txt;
	@echo "Result:";
	@printf "\n";
	@cat tests/bisection_recursive.txt | ./exp_test.native;

clean:
	@echo "Removing build files.";
	@rm -f exp_test.native;
	@rm -rf _build/;