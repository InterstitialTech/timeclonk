{ config, options, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.timeclonk;
  opt = options.services.timeclonk;
  settingsFormat = pkgs.formats.toml { };

in

{

  ###### interface
  options = {
    services.timeclonk = {
      enable = mkEnableOption (lib.mdDoc "timeclonk; markdown based multi user zettelkasten");

      user = mkOption {
        type = types.str;
        default = "timeclonk";
        example = "timeclonk-user";
        description = "linux user account in which to run timeclonk.";
      };
      group = lib.mkOption {
        type = lib.types.str;
        default = "timeclonk";
        description = lib.mdDoc "linux group under which timeclonk runs.";
      };

    settings = lib.mkOption {
      inherit (settingsFormat) type;
      default = ''
        ip = '127.0.0.1'
        port = 8000
        createdirs = true
        altmainsite = []
        file_tmp_path = './temp'
        file_path = './files'

        [orgauth_config]
        mainsite = 'http://localhost:8000'
        appname = 'timeclonk'
        emaildomain = 'timeclonk.com'
        db = './timeclonk.db'
        admin_email = 'admin@admin.admin'
        regen_login_tokens = true
        email_token_expiration_ms = 86400000
        reset_token_expiration_ms = 86400000
        invite_token_expiration_ms = 604800000
        open_registration = false
        send_emails = false
        non_admin_invite = true
        remote_registration = true
      '';
      description = ''
        timeclonk config.toml file.
      '';
    };

      listenPort = mkOption {
        type = types.nullOr types.int;
        default = null;
        example = 8011;
        description = "Listen on a specific IP port.";
      };

    };
  };

  ###### implementation
  config = mkIf cfg.enable {

    environment.systemPackages = [ pkgs.typst ];

    systemd.services.timeclonk = {
      description = "timeclonk";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig.User = cfg.user;
      serviceConfig.Group = cfg.group;

      script = ''
        cd "/home/${cfg.user}"
        mkdir -p timeclonk
        cd timeclonk
        echo "${pkgs.typst}/bin/typst" > typst
        echo "${cfg.settings}" > config.toml
        RUST_LOG=info ${pkgs.timeclonk}/bin/timeclonk-server -c config.toml
        '';
    };

    users.groups = {
      ${cfg.group} = { };
    };

    users.users = lib.mkMerge [
      (lib.mkIf (cfg.user == "timeclonk") {
        ${cfg.user} = {
          # isSystemUser = true;
          group = cfg.group;
          home = "/home/${cfg.user}";
          createHome = true;
        };
      })
    ];
  };
}
