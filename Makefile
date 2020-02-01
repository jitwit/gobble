.PHONY : clean all

dictionary::= input/trie.fasl

all : $(dictionary)

input/trie.fasl : code/trie.so
	scheme --script load.scm

code/trie.so : code/trie.scm
	echo "(compile-library \"$<\")" | scheme -q

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
