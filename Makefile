actions::= dry-update-gobble update-gobble update-boards update-static ghcid-gobble update-images
static-dir::= /var/www/gobble/static
image-dir::= /var/www/gobble/images

.PHONY : clean $(actions)

# shortcuts
dry-update-gobble :
	nixops modify nibble/aws.nix nibble/app.nix -d gobbler
	nixops deploy -d gobbler --dry-run

update-gobble :
	nixops modify nibble/aws.nix nibble/app.nix -d gobbler
	nixops deploy -d gobbler

update-static :
	nixops scp --to gobble-net garcon/static/gobble.css $(static-dir)
	nixops scp --to gobble-net garcon/static/gobble.js $(static-dir)
	nixops scp --to gobble-net garcon/static/jquery-3.4.1.slim.js $(static-dir)

update-images :
	nixops scp --to gobble-net garcon/images/ $(image-dir)

ghcid-gobble :
	cd garcon && nix-shell --command 'ghcid -c "cabal repl gobble" -s ":l Garcon" -r'

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
