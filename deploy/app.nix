{
  network.description = "gobble-net";
  gobble-net = { config, pkgs, ... }:
    let gobbler = import ../garcon/default.nix {};
        hemlock = pkgs.callPackage ./chez-hemlock.nix {} ;
        euler   = pkgs.callPackage ./chez-euler.nix { chez-hemlock = hemlock; };
    in
  {

    networking.firewall = {
      allowedTCPPorts = [ 22 80 ];
      enable = false; };

    environment.systemPackages =
      [ gobbler
        pkgs.chez
        pkgs.chez-srfi
        hemlock
        euler
      ];

    systemd.services.gobble =
    { description = "gobble";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig =
      { WorkingDirectory = "/var/www/gobble";
        ExecStart = "${gobbler}/bin/gobble 80"; }; };   
  };
}
