{ network.description = "gobble-net";
  gobble-net = { config, pkgs, ... }:
let gobbler = import ../garcon/default.nix {};
    hemlock = pkgs.callPackage ./chez-hemlock.nix {} ;
    euler   = pkgs.callPackage ./chez-euler.nix { chez-hemlock = hemlock; };
    cobble  = pkgs.callPackage ./chez-gobble.nix
      { chez-hemlock = hemlock; chez-euler = euler; };
    chezlibs = [ pkgs.chez-srfi pkgs.chez-matchable euler hemlock cobble ];
    libdirs = builtins.concatStringsSep ":"
      (map (x: "${x}/lib/csv9.5-site") chezlibs);
in
{ networking.firewall = {
    allowedTCPPorts = [ 22 80 ];
    enable = false; };

  environment.systemPackages =
    [ gobbler
      pkgs.chez
      pkgs.chez-srfi
      pkgs.chez-matchable
      hemlock
      euler
      cobble ];
  
  systemd.services.cobble =
    { description = "make board for gobble";
      wantedBy = [ "multi-user.target" ];
      environment = { CHEZSCHEMELIBDIRS = "${libdirs}"; };
      serviceConfig =
        { WorkingDirectory = "${cobble}";
          ExecStart = "${cobble}/bin/gobbler -n 1000 -d /var/www/gobble/boards";
        };
    };

  systemd.services.gobble =
    { description = "boggle-bitch.net";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "cobble.service" ];
      wants = [ "cobble.service" ];
      serviceConfig =
        { WorkingDirectory = "/var/www/gobble";
          ExecStart = "${gobbler}/bin/gobble 80";
        };
    };
  };
}
