all: 
	@ocamlbuild -use-menhir -use-ocamlfind -r src/exp_test.native;

own_test:  
	@echo "Enter path to test file (.txt only); E.g. tests/test1.txt:"; \
	read path; \
	./exp_test.native $$path

all_tests:
	@echo  "Test1:";
	@printf "\n";
	@cat tests/test1.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test1.txt;
	@printf "\n";
	@echo  "Test2:";
	@printf "\n";
	@cat tests/test2.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test2.txt;
	@printf "\n";
	@echo "Test3:";
	@printf "\n";
	@cat tests/test3.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test3.txt;
	@printf "\n";
	@echo "Test4:";
	@printf "\n";
	@cat tests/test4.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test4.txt;
	@printf "\n";
	@echo "Test5:";
	@printf "\n";
	@cat tests/test5.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test5.txt;
	@printf "\n";
	@echo "Test6:";
	@printf "\n";
	@cat tests/test6.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test6.txt;
	@printf "\n";
	@echo "Test7:";
	@printf "\n";
	@cat tests/test7.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test7.txt; 
	@printf "\n";
	@echo "Test8:";
	@printf "\n";
	@cat tests/test8.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test8.txt;
	@printf "\n";
	@echo "Test9:";
	@printf "\n";
	@cat tests/test9.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test9.txt;
	@printf "\n";
	@echo "Test10:";
	@printf "\n";
	@cat tests/test10.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/test10.txt;
	@printf "\n";
	@echo "Iterative bisection:";
	@printf "\n";
	@cat tests/bisection_iterative.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/bisection_iterative.txt;
	@printf "\n";
	@echo "Recursive bisection:";
	@printf "\n";
	@cat tests/bisection_recursive.txt;
	@echo "Result:";
	@printf "\n";
	@./exp_test.native tests/bisection_recursive.txt;

clean:
	@echo "Removing build files.";
	@rm -f exp_test.native;
	@rm -rf _build/;