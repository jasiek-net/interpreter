all:
	cd bnfc && $(MAKE)
	ghc --make Interpreter.hs -i./bnfc -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi interpreter
	cd bnfc && $(MAKE) clean
	
