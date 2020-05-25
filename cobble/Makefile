dictionary::= input/trie.fasl
objs::= trie dictionary gobble
lib-path::= .:$(CHEZSCHEMELIBDIRS)
scheme::= scheme -q --libdirs $(lib-path)
libs::= $(foreach lib,$(objs),$(lib).so)
boards::= 1000
local-boards::= ../garcon/boards

.PHONY : clean all boards

all : $(dictionary) $(libs)

# scheme libraries
gobble.so : code/*.scm dictionary.so trie.so *.sls
	echo "(compile-library \"gobble.sls\")" | $(scheme)

dictionary.so : code/*.scm trie.so *.sls
	echo "(compile-library \"dictionary.sls\")" | $(scheme)

trie.so : code/*.scm *.sls
	echo "(compile-library \"trie.sls\")" | $(scheme)

# english dictionary, binary file
$(dictionary) : $(libs)
	$(scheme) --script load.scm

boards : gobbler.ss $(libs) input/trie.fasl
	rm -rf $(local-boards)
	mkdir -p $(local-boards)
	$(scheme) --script $< -n $(boards) -d $(local-boards)

update-boards :
	nixops scp --to gobble-net $(local-boards) /var/www/gobble

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
