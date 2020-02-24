{
  network.description = "gobble-net";
  gobble-net = { config, pkgs, ... }:
  let
    gobbler  = import ../garcon/default.nix {};
    openssl  = pkgs.openssl;
    domain   = "mmoogle.ca";
  in
  {

    networking.firewall = {
      allowedTCPPorts = [ 22 80 ];
      enable = false;
    };

    environment.systemPackages =
      [ openssl
        gobbler
      ];

    systemd.services.gobble =
    { description = "gobble";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig =
      { WorkingDirectory = "/var/www/gobble";
        ExecStart = "${gobbler}/bin/gobble 80";
      };
    };
    
  };
}
