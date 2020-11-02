{ network.description = "gobble-net";
  gobble-net = { config, pkgs, stdenv, ... }: with pkgs; with builtins; with lib; with lists;
let gobbler  = import ../garcon/default.nix {};
#    hemlock  = callPackage ./chez-hemlock.nix {} ;
#    euler    = callPackage ./chez-euler.nix { chez-hemlock = hemlock; };
#    cobble   = callPackage ./chez-gobble.nix { chez-hemlock = hemlock; chez-euler = euler; };
#    chezlibs = [ chez-srfi chez-matchable euler hemlock cobble ];
#    libdirs  = concatStringsSep ":" (map (x: "${x}/lib/csv9.5-site") chezlibs);
    webdir   = "/var/www/gobble";
in {
  networking.firewall = { allowedTCPPorts = [ 22 80 ]; enable = false; };
  environment.systemPackages = flatten [ gobbler sqlite ];
  systemd = {
    services = {
      gobble = {
        description = "boggle-bitch.net";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
#        environment = { CHEZSCHEMELIBDIRS = "${libdirs}"; };
        serviceConfig =
          { WorkingDirectory = webdir;
            ExecStart = "${gobbler}/bin/garcon -p 80";
            Restart = "always"; }; }; }; }; }; }
