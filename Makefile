.PHONY : clean

code/trie.so : code/trie.scm
	echo "(compile-library \"$<\")" | scheme

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
