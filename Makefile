.PHONY : clean all boards input/frequencies.txt

dictionary::= input/trie.fasl
objs::= trie dictionary gobble
lib-path::= .:$(CHEZSCHEMELIBDIRS)
scheme::= scheme -q --libdirs $(lib-path)
libs::= $(foreach lib,$(objs),$(lib).so)
reps::= 1000

all : $(dictionary) $(libs)

input/trie.fasl : $(libs)
	$(scheme) --script load.scm

boards : gobbler.ss $(libs) input/trie.fasl
	$(scheme) --script $< -n 10000 -d garcon/boards

gobble.so : code/*.scm dictionary.so trie.so *.sls
	echo "(compile-library \"gobble.sls\")" | $(scheme)

dictionary.so : code/*.scm trie.so *.sls
	echo "(compile-library \"dictionary.sls\")" | $(scheme)

trie.so : code/*.scm *.sls
	echo "(compile-library \"trie.sls\")" | $(scheme)

input/frequencies.txt : etude.ss
	$(scheme) --script $< $(reps) > $@

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
