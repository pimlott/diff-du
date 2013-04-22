diff-du: diff-du.hs
	ghc -o diff-du diff-du.hs

clean:
	rm *.hi *.o diff-du
