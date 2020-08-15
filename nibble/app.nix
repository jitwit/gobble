{ network.description = "gobble-net";
  gobble-net = { config, pkgs, stdenv, ... }: with pkgs; with builtins; with lib; with lists;
let gobbler  = import ../garcon/default.nix {};
    hemlock  = callPackage ./chez-hemlock.nix {} ;
    euler    = callPackage ./chez-euler.nix { chez-hemlock = hemlock; };
    cobble   = callPackage ./chez-gobble.nix { chez-hemlock = hemlock; chez-euler = euler; };
    chezlibs = [ chez-srfi chez-matchable euler hemlock cobble ];
    libdirs  = concatStringsSep ":" (map (x: "${x}/lib/csv9.5-site") chezlibs);
    webdir   = "/var/www/gobble";
in {
  networking.firewall = { allowedTCPPorts = [ 22 80 ]; enable = false; };
  environment.systemPackages = flatten [ gobbler chez chezlibs ];
  systemd = {
    timers.cobble = {
      wantedBy = [ "timers.target" ];
      partOf = [ "cobble.service" ];
      timerConfig.OnCalendar = "daily"; };
    services = {
       cobble = {
         description = "make board for gobble";
         wantedBy = [ "multi-user.target" ];
         environment = { CHEZSCHEMELIBDIRS = "${libdirs}"; };
         serviceConfig = {
           WorkingDirectory = "${cobble}";
           ExecStart = "${cobble}/bin/gobbler -n 1000 -d ${webdir}/boards"; }; };
      gobble = {
        description = "boggle-bitch.net";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "cobble.service" ];
        wants = [ "cobble.service" ];
        environment = { CHEZSCHEMELIBDIRS = "${libdirs}"; };
        serviceConfig =
          { WorkingDirectory = webdir;
            ExecStart = "${gobbler}/bin/garcon -p 80 -g ${cobble}";
            Restart = "always"; }; }; }; }; }; }
