.PHONY : clean all

dictionary::= input/trie.fasl
objs::= code/trie.so code/dictionary.so code/gobble.so
lib-path::= code:$(CHEZSCHEMELIBDIRS)
scheme::= scheme -q --libdirs $(lib-path)

all : $(dictionary) $(objs)

input/trie.fasl : $(objs)
	scheme --script load.scm

code/gobble.so : code/gobble.scm code/dictionary.so code/trie.so
	echo "(compile-library \"$<\")" | $(scheme)

code/dictionary.so : code/dictionary.scm code/trie.so
	echo "(compile-library \"$<\")" | $(scheme)

code/trie.so : code/trie.scm
	echo "(compile-library \"$<\")" | $(scheme)

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
