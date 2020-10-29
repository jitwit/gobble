actions = dry-update-gobble update-gobble update-boards update-static ghcid-gobble update-images
static-dir = /var/www/gobble/static

.PHONY : clean $(actions)

# shortcuts
dry-update-gobble :
	nixops modify nibble/aws.nix nibble/app.nix -d gobbler
	nixops deploy -d gobbler --dry-run

update-gobble :
	nixops modify nibble/aws.nix nibble/app.nix -d gobbler
	nixops deploy -d gobbler
	nixops ssh -d gobbler gobble-net systemctl status gobble
#	nixops ssh -d gobbler gobble-net systemctl status cobble

update-boards :
	cd ../joggle && rm boards.txt && make boards.txt
	nixops scp --to gobble-net ../joggle/boards.txt $(static-dir)

update-static :
#	nixops scp --to gobble-net garcon/static/icon.png $(static-dir)
	nixops scp --to gobble-net garcon/static/boards.txt $(static-dir)
#	nixops scp --to gobble-net garcon/static/dawggle.dawg $(static-dir)
	nixops scp --to gobble-net garcon/static/gobble.css $(static-dir)
	nixops scp --to gobble-net garcon/static/gobble.js $(static-dir)
#	nixops scp --to gobble-net garcon/static/jquery-3.4.1.slim.js $(static-dir)
#	nixops scp --to gobble-net garcon/static/definitions.txt $(static-dir)
#	nixops scp --to gobble-net garcon/static/collins.fasl $(static-dir)

update-images :
	nixops scp --to gobble-net garcon/images/ $(static-dir)

ghcid-gobble :
	cd garcon && nix-shell --command 'ghcid -c "cabal repl gobble" -s ":l Garcon" -r'

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
