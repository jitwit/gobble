actions = dry-update-gobble update-gobble update-static ghcid-gobble update-images download-db
static-dir = /var/www/gobble/static

.PHONY : clean $(actions)

# shortcuts
ghcid-gobble :
	cd garcon && nix-shell --command 'ghcid -c "cabal repl gobble" -s ":l Garcon" -r'

dry-update-gobble :
	nixops modify nibble/aws.nix nibble/app.nix -d gobbler
	nixops deploy -d gobbler --dry-run

update-gobble :
	nixops modify nibble/aws.nix nibble/app.nix -d gobbler
	nixops deploy -d gobbler
	nixops scp --to gobble-net garcon/static/gobble.css $(static-dir)
	nixops scp --to gobble-net garcon/static/gobble.js $(static-dir)
	nixops ssh -d gobbler gobble-net systemctl status gobble

update-static :
	nixops scp --to gobble-net garcon/static/gobble.css $(static-dir)
	nixops scp --to gobble-net garcon/static/gobble.js $(static-dir)
	nixops scp --to gobble-net garcon/static/Apl385.ttf $(static-dir)
#	nixops scp --to gobble-net garcon/static/icon.png $(static-dir)
#	nixops scp --to gobble-net garcon/static/boards.txt $(static-dir)
#	nixops scp --to gobble-net garcon/static/dawggle.dawg $(static-dir)
#	nixops scp --to gobble-net garcon/static/jquery-3.4.1.slim.js $(static-dir)
#	nixops scp --to gobble-net garcon/static/definitions.txt $(static-dir)
#	nixops scp --to gobble-net garcon/static/collins.fasl $(static-dir)

update-images :
	nixops scp --to gobble-net garcon/images/ $(static-dir)

download-db :
	scp root@boggle-bitch.net:/var/www/gobble/data/gobble.db gobble.db

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
