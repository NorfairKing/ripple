{ nixosTest
, ripple-nixos-module-factory
}:
let
  ripple-production = ripple-nixos-module-factory {
    envname = "production";
  };
  port = 8001;
in
nixosTest (
  { lib, pkgs, ... }: {
    name = "ripple-module-test";
    nodes = {
      server = {
        imports = [
          ripple-production
        ];
        services.ripple.production = {
          enable = true;
          server = {
            enable = true;
            inherit port;
          };
        };
      };
    };
    testScript = ''
      server.start()
      server.wait_for_unit("multi-user.target")

      # server.wait_for_open_port(${builtins.toString port})
      # server.succeed("curl localhost:${builtins.toString port}")
    '';
  }
)
