actions::= dry-update-gobble update-gobble update-boards update-static ghcid-gobble update-images

.PHONY : clean $(actions)

# shortcuts
dry-update-gobble :
	nixops modify nibble/aws.nix nibble/app.nix -d gobbler
	nixops deploy -d gobbler --dry-run

update-gobble :
	nixops modify nibble/aws.nix nibble/app.nix -d gobbler
	nixops deploy -d gobbler

update-boards : boards
	nixops scp --to gobble-net garcon/boards/ /var/www/gobble

update-static :
	nixops scp --to gobble-net garcon/static/ /var/www/gobble

update-images :
	nixops scp --to gobble-net garcon/images/ /var/www/gobble/static

ghcid-gobble :
	cd garcon && nix-shell --command 'ghcid -c "cabal repl gobble" -s ":l Main" -r'

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
