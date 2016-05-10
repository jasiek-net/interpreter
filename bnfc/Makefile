all:
	happy -gca ParJC.y
	alex -g LexJC.x
	ghc --make TestJC.hs -o TestJC

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocJC.* LexJC.* ParJC.* LayoutJC.* SkelJC.* PrintJC.* TestJC.* AbsJC.* TestJC ErrM.* SharedString.* ComposOp.* JC.dtd XMLJC.* Makefile*
	
