.PHONY : clean all

dictionary::= input/trie.fasl
objs::= trie dictionary gobble
lib-path::= .:$(CHEZSCHEMELIBDIRS)
scheme::= scheme -q --libdirs $(lib-path)
libs::= $(foreach lib,$(objs),$(lib).so)

all : $(dictionary) $(libs)

input/trie.fasl : $(libs)
	$(scheme) --script load.scm

gobble.so : code/*.scm dictionary.so trie.so *.sls
	echo "(compile-library \"gobble.sls\")" | $(scheme)

dictionary.so : code/*.scm trie.so *.sls
	echo "(compile-library \"dictionary.sls\")" | $(scheme)

trie.so : code/*.scm *.sls
	echo "(compile-library \"trie.sls\")" | $(scheme)

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
