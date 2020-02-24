.PHONY : clean all boards input/frequencies.txt update-gobble update-boards update-static

dictionary::= input/trie.fasl
objs::= trie dictionary gobble
lib-path::= .:$(CHEZSCHEMELIBDIRS)
scheme::= scheme -q --libdirs $(lib-path)
libs::= $(foreach lib,$(objs),$(lib).so)
boards::= 1000

all : $(dictionary) $(libs)

input/trie.fasl : $(libs)
	$(scheme) --script load.scm

boards : gobbler.ss $(libs) input/trie.fasl
	rm -rf garcon/boards
	mkdir -p garcon/boards
	$(scheme) --script $< -n $(boards) -d garcon/boards

gobble.so : code/*.scm dictionary.so trie.so *.sls
	echo "(compile-library \"gobble.sls\")" | $(scheme)

dictionary.so : code/*.scm trie.so *.sls
	echo "(compile-library \"dictionary.sls\")" | $(scheme)

trie.so : code/*.scm *.sls
	echo "(compile-library \"trie.sls\")" | $(scheme)

input/frequencies.txt : etude.ss
	$(scheme) --script $< $(reps) > $@

update-gobble :
	nixops modify deploy/aws.nix deploy/app.nix -d gobbler
	nixops deploy -d gobbler

update-boards : boards
	nixops scp --to gobble-net garcon/boards/ /var/www/gobble

update-static :
	nixops scp --to gobble-net garcon/static/ /var/www/gobble

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
