{ ripple-server
}:
{ envname
}:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.ripple."${envname}";

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
in
{
  options.services.ripple."${envname}" = {
    enable = mkEnableOption "Ripple Service";
    server = mkOption {
      type = types.submodule {
        options = {
          enable = mkEnableOption "Ripple API Server";
          config = mkOption {
            default = { };
            description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
          };
          log-level = mkOption {
            type = types.str;
            example = "Debug";
            default = "Warn";
            description = "The log level to use";
          };
          hosts = mkOption {
            type = types.listOf (types.str);
            default = [ ];
            example = "api.ripple.cs-syd.eu";
            description = "The host to serve api requests on";
          };
          port = mkOption {
            type = types.int;
            example = 8001;
            description = "The port to serve api requests on";
          };
        };
      };
      default = null;
    };
  };
  config =
    let
      working-dir = "/www/ripple/${envname}/";
      attrOrNull = name: value: optionalAttrs (!builtins.isNull value) { "${name}" = value; };
      server-config = with cfg.server; mergeListRecursively [
        (attrOrNull "port" port)
        (attrOrNull "log-level" log-level)
        cfg.server.config
      ];
      server-config-file = (pkgs.formats.yaml { }).generate "ripple-server-config.yaml" server-config;
      # The docs server
      server-working-dir = working-dir + "server/";
      server-database-file = server-working-dir + "ripple-server.sqlite3";
      # The api server
      server-service =
        with cfg.server;
        optionalAttrs enable {
          "ripple-server-${envname}" = {
            description = "Ripple API Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "RIPPLE_SERVER_CONFIG_FILE" = "${server-config-file}";
              };
            script =
              ''
                mkdir -p "${server-working-dir}"
                cd ${server-working-dir};
                ${ripple-server}/bin/ripple-server
              '';
            serviceConfig =
              {
                Restart = "always";
                RestartSec = 1;
                Nice = 15;
              };
            unitConfig =
              {
                StartLimitIntervalSec = 0;
                # ensure Restart=always is always honoured
              };
          };
        };
      server-host =
        with cfg.server;

        optionalAttrs (enable && hosts != [ ]) {
          "${head hosts}" = {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              proxyPass = "http://localhost:${builtins.toString port}";
            };
            serverAliases = tail hosts;
          };
        };
    in
    mkIf cfg.enable {
      systemd.services =
        mergeListRecursively [
          server-service
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional cfg.server.enable cfg.server.port)
      ];
      services.nginx.virtualHosts =
        mergeListRecursively [
          server-host
        ];
    };
}
