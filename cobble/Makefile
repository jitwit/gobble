dictionary = input/trie.fasl
objs = dictionary gobble dawg
lib-path = .:$(CHEZSCHEMELIBDIRS)
scheme = scheme -q --optimize-level 3 --libdirs $(lib-path)
libs = $(foreach lib,$(objs),$(lib).so)
boards = 1000
local-boards = ../garcon/boards

.PHONY : clean all boards

all : $(dictionary) $(libs)

# scheme libraries
gobble.so : code/*.scm dictionary.so *.sls
	echo "(compile-library \"gobble.sls\")" | $(scheme)

dictionary.so : code/*.scm *.sls
	echo "(compile-library \"dictionary.sls\")" | $(scheme)

dawg.so : code/*.scm *.sls
	echo "(compile-library \"dawg.sls\")" | $(scheme)

gobbler.so : gobbler.ss dawg.so gobble.so dictionary.so $(dictionary)
	echo "(compile-script \"$<\")" | $(scheme)

# english dictionary, binary file
$(dictionary) : $(libs)
	$(scheme) --script load.scm

boards : gobbler.so
	rm -rf $(local-boards)
	mkdir -p $(local-boards)
	$(scheme) --script $< -n $(boards) -d $(local-boards)

# obsolete
update-boards :
	nixops scp --to gobble-net $(local-boards) /var/www/gobble

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
