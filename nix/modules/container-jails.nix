{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.container-jails;
  firstToUpper = v: lib.concatImapStrings (pos: k: if pos > 1 then k else lib.toUpper k) (lib.stringToCharacters v);
in

{
  meta.maintainers = [ "cidkid" ];

  options = {
    container-jails = {
      enable = mkEnableOption "container-jails";

      network = {
        interfaces = mkOption {
          type = types.nullOr (types.listOf types.str);
          default = [];
        };
      };

      containers = mkOption {
        type = types.attrsOf
          (types.submodule ({ name, config, ... }: {
            options = {
              pkgs = mkOption {
                type = types.attrs;
                default = pkgs;
              };

              containerUser = {
                uid = mkOption {
                  type = types.int;
                  default = 1000;
                };
                shell = mkOption {
                  type = types.package;
                  default = pkgs.bash;
                };
                extraGroups = mkOption {
                  type = types.listOf types.str;
                  default = [];
                };
              };

              limits = {
                memory = {
                  enable = mkEnableOption "memory.enable";
                  max = mkOption {
                    type = types.string;
                    default = {};
                  };
                };
                cpu = {
                  enable = mkEnableOption "cpu.enable";
                  allowedCPUs = mkOption {
                    type = types.listOf types.int;
                    default = {};
                  };
                };
              };

              extra = {
                systemConfig = mkOption {
                  type = types.attrs;
                  default = { };
                };

                syscalls = mkOption {
                  type = types.listOf types.str;
                  default = [ ];
                };

                drop-caps = mkOption {
                  type = types.listOf types.str;
                  default = [ ];
                };

                keep-caps = mkOption {
                  type = types.listOf types.str;
                  default = [];
                };

                home-manager-setup = mkEnableOption "home-manager-setup";

                allowedDevices = mkOption {
                  type = types.listOf (types.submodule ({ ... }: {
                    options = {
                      node = mkOption {
                        example = types.str;
                      };
                      modifier = mkOption {
                        type = types.str;
                      };
                    };
                  }));
                  default = [ ];
                };

                link-user-extras = mkOption {
                  type = types.str;
                  default = "";
                };

                bindMounts = mkOption {
                  type = types.listOf (types.submodule ({ ... }: {
                    options = {
                      host-directory = mkOption {
                        type = types.str;
                      };

                      container-directory = mkOption {
                        type = types.str;
                      };

                      readonly = mkOption {
                        type = types.bool;
                        default = false;
                      };
                    };
                  }));
                  default = [ ];
                };

                home-binds = mkOption {
                  type = types.listOf (types.submodule ({ ... }: {
                    options = {
                      path = mkOption {
                        type = types.str;
                      };

                      rebind-host-path = mkOption {
                        type = types.nullOr types.str;
                        default = null;
                      };

                      user = mkOption {
                        type = types.str;
                      };

                      container-user = mkOption {
                        type = types.str;
                        default = "container";
                      };

                      type = mkOption {
                        type = types.strMatching "file|directory";
                        default = "directory";
                      };

                      readOnly = mkOption {
                        type = types.bool;
                        default = false;
                      };
                    };
                  }));
                  default = [ ];
                };
              };

              stateVersion = mkOption {
                type = types.str;
                default = "21.11";
              };

              network = {
                disable = mkEnableOption "network.disable";
                private = mkEnableOption "network.private";

                macvlans = mkOption {
                  type = types.listOf types.str;
                  default = [ ];
                };

                localAddress = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                };

                hostAddress = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                };
              };

              isolate = {
                networking-to-container = mkEnableOption "networking-to-container";
                interfaces = mkOption {
                  type = types.listOf types.str;
                  default = [ ];
                };
              };

              inputs = {
                enable = mkEnableOption "inputs.enable";
              };

              dbus = rec {
                share = mkEnableOption "dbus.share";
                user = mkOption {
                  type = types.int;
                  default = 1000;
                };
                settings = {
                  talk = mkOption {
                    type = types.listOf types.str;
                    default = [];
                  };
                  see = mkOption {
                    type = types.listOf types.str;
                    default = [];
                  };
                  own = mkOption {
                    type = types.listOf types.str;
                    default = [];
                  };
                  call = mkOption {
                    type = types.listOf (types.submodule ({ ... }: {
                      options = {
                        interface = mkOption {
                          type = types.str;
                        };

                        rule = mkOption {
                          type = types.str;
                        };
                      };
                    }));
                    default = [];
                  };
                  broadcast = mkOption {
                    type = types.listOf (types.submodule ({ ... }: {
                      options = {
                        interface = mkOption {
                          type = types.str;
                        };

                        rule = mkOption {
                          type = types.str;
                        };
                      };
                    }));
                    default = [];
                  };
                };
              };

              audio = {
                pulseaudio = {
                  enable = mkEnableOption "pulse";
                  socket = mkOption {
                    type = types.str;
                    default = "/run/user/1000/pulse";
                  };
                };
                pipewire = {
                  enable = mkEnableOption "pipewire";
                  setup-alsa = mkEnableOption "setup-alsa";

                  pulse-socket = mkOption {
                    type = types.str;
                    default = "/run/user/1000/pulse/native";
                  };

                  socket = mkOption {
                    type = types.str;
                    default = "/run/user/1000/pipewire-0";
                  };
                };
              };

              gpu = {
                accel = mkEnableOption "gpu.accel";
                nvidia = mkEnableOption "gpu.nvidia";
                x11 = {
                  enable = mkEnableOption "gpu.x11.enable";
                  display-var = mkOption {
                    type = types.str;
                    default = ":0";
                  };
                };

                wayland = {
                  enable = mkEnableOption "gpu.wayland.enable";

                  socket = mkOption {
                    type = types.str;
                    default = "/run/user/1000/wayland-0";
                  };
                };
              };

              apps = mkOption {
                type = types.listOf (types.submodule ({ ... }: {
                  options = {
                    package = mkOption {
                      type = types.package;
                    };

                    generateDesktopFile = mkOption {
                      type = types.bool;
                      default = false;
                    };

                    generateShim = mkOption {
                      type = types.bool;
                      default = false;
                    };

                    exe-name = mkOption {
                      type = types.nullOr types.str;
                      default = null;
                    };

                    desktopName = mkOption {
                      type = types.nullOr types.str;
                      default = null;
                    };
                  };
                }));
              };

            };
          }));
      };
    };
  };

  config =
    let
      enter-container = pkgs.writeShellScriptBin "enter-container" ''
        machinectl shell --quiet --uid=$UID $1
      '';

      container-shim = pkgs.writeShellScriptBin "container-shim" ''
        source /etc/profile
        args=$(echo "''${@:2}")
        if [[ $args == "" ]]; then
          exec /run/current-system/sw/bin/$1
        else
          exec /run/current-system/sw/bin/$1 $args
        fi
      '';

      container-program = pkgs.writeShellScriptBin "container-program" ''
        machinectl shell --quiet --uid=1000 $1 ${container-shim}/bin/container-shim "''${@:2}"
      '';

      mkContainerAutoStartGUI = { dbus_dep ? { container = null; enable = false; } }: ({
        unitConfig = {
          StartLimitBurst = "infinity";
          StartLimitIntervalSec = 5;
        };

      } // lib.optionalAttrs (dbus_dep.enable) {
        # If the dbus settings change we have to trigger a restart or DBUS will drop
        # out of the container scope due to the state that it managers
        partOf = [ "container@${dbus_dep.container}-dbus-proxy.service" ];
      });


      limitResources = { cpu ? { enable = false; }, mem ? { enable = false; } }: {
        serviceConfig = {
          Restart = lib.mkForce "always";
          RestartSec = 15;
        } // lib.optionalAttrs (cpu.enable) {
          CPUAccounting = true;
          AllowedCPUs = builtins.concatStringsSep "," (builtins.map builtins.toString cpu.allowedCPUs);
        } // lib.optionalAttrs (mem.enable) {
          MemoryAccounting = true;
          MemoryMax = mem.max;
        };
      };
    in
    mkIf cfg.enable {
      # NOTE(Dylan Green): Generate desktop-files for host system, depending on if the user wants a
      # desktop file for the specified program
      environment.systemPackages = builtins.concatLists [
        (builtins.concatLists (map
          (c:
            let
              prg = map
                (x:
                   let
                     maybePackageName = builtins.toString (builtins.head (builtins.splitVersion x.package.name));
                     parseExe =
                       if (!isNull x.exe-name) then
                         x.exe-name
                       else if x.package ? pname then
                         x.package.pname
                       else maybePackageName;
                     dName = if (!isNull x.desktopName) then
                         x.desktopName
                       else parseExe;
                     desktopItem = pkgs.makeDesktopItem {
                       name = "(Container ${c}) ${firstToUpper dName}";
                       desktopName = "(Container ${c}) ${firstToUpper dName}";
                       exec = "${container-program}/bin/container-program ${c} ${parseExe}";
                     };
                     localScript = pkgs.writeShellScriptBin "container-${c}-${parseExe}" ''
                       ${container-program}/bin/container-program ${c} ${parseExe} $@
                     '';
                   in
                   pkgs.runCommand "container-${c}-shim-${dName}" { } (''
                     mkdir -p $out
                     echo "container-${c}-shim-${dName}" >> $out/shim
                   '' + lib.optionalString (x.generateDesktopFile) "install -Dm444 -t $out/share/applications/ ${desktopItem}/share/applications/*.desktop"
                      + lib.optionalString (x.generateShim) ''
                          mkdir -p $out/bin
                          cp ${localScript}/bin/* $out/bin
                      '')
                )
                cfg.containers."${c}".apps;
            in
            prg
          )
          (builtins.attrNames cfg.containers)))
        [ enter-container ]
      ] ++ [ pkgs.container-manager ];

      systemd.services = {
        "container-manager" = {
          enable = true;
          wantedBy = [ "multi-user.target" ];
          path = [ pkgs.util-linux ];
          serviceConfig = {
            Type = "simple";
            ExecStart = "${pkgs.container-manager}/bin/server";
          };
        };
      } // builtins.listToAttrs
        (map
        (e: let
          container = cfg.containers."${e}";
        in {
            name = "container@${e}";
            value = (mkContainerAutoStartGUI { dbus_dep = { enable = container.dbus.share; container = e; }; }) // (limitResources { cpu = container.limits.cpu; mem = container.limits.memory; });
          })
          (builtins.attrNames cfg.containers))
      // (builtins.listToAttrs (map
      (e: let
        value = cfg.containers."${e}";
        createCliOptions = a: name: (builtins.map (x: "--${name}=${x}") a);
        createRuleOption = a: name: (builtins.map (x: "--${name}=${x.interface}=${x.rule}") a);
        cliOptions =
          createCliOptions value.dbus.settings.talk "talk"
          ++ createCliOptions value.dbus.settings.see "see"
          ++ createCliOptions value.dbus.settings.own "own"
          ++ createRuleOption value.dbus.settings.call "call"
          ++ createRuleOption value.dbus.settings.broadcast "broadcast";
      in {
          name = "container@${e}-dbus-proxy";
          value = {
            enable = value.dbus.share;
            wantedBy = [ "container@${e}.service" ];
            serviceConfig = {
              Type = "simple";
              User = toString value.dbus.user;
            };
            path = with pkgs; [ coreutils-full ];
            script = ''
              mkdir -p /tmp/dbus-${e}
              ${pkgs.xdg-dbus-proxy}/bin/xdg-dbus-proxy unix:path=/run/user/${toString value.dbus.user}/bus /tmp/dbus-${e}/dbus-proxy --filter --log ${builtins.concatStringsSep " " cliOptions}
            '';
          };
        }) (builtins.attrNames cfg.containers)))
      // (builtins.listToAttrs (map
      (x: let
        udev = ''
          mkdir -p /yacc/${x}
          mkdir -p /yacc/${x}/udev
        '';
      in {
        name = "container@${x}-udev";
        value = {
          enable = true;
          wantedBy = [ "mutli-user.target" "container@${x}.service" ];
          serviceConfig = {
            RemainAfterExit = true;
            Type = "oneshot";
          };
          script = udev;
        };
      }) (builtins.attrNames cfg.containers)))
      // (builtins.listToAttrs (map
      (e: let
        invalidFiles = x:
             x == "/etc"
          || x == "/etc/shadow";
      in {
          name = "container@${e}-setup";
          value = {
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              RemainAfterExit = true;
            };
            script = builtins.concatStringsSep "\n"
              (builtins.concatLists [
                (map (x: "mkdir -p ${x.host-directory}") (builtins.filter (d: invalidFiles d.host-directory) cfg.containers."${e}".extra.bindMounts))
                (map
                  (c:
                    let
                      path = if !(isNull c.rebind-host-path)
                        then "/home/${c.user}/${c.rebind-host-path}"
                        else "/home/${c.user}/${c.path}";

                      directory = lib.optionalString (c.type == "directory") ''
                        if [ ! -d ${path} ]
                        then
                          mkdir -p ${path}
                          chown -R ${c.user} ${path}
                        else
                          echo "Directory ${path} Exists!"
                        fi
                      '';
                      file = lib.optionalString (c.type == "file") ''
                        if [ ! -f ${path} ]
                        then
                          touch ${path}
                          chown -R ${c.user} ${path}
                        else
                          echo "File ${path} Exists!"
                        fi
                      '';
                    in
                    directory + file)
                  (cfg.containers."${e}".extra.home-binds))
              ]) + lib.optionalString (cfg.containers."${e}".extra.home-manager-setup) "mkdir -p /tmp/${e}/per-user-{profile,gcroots}";

            serviceConfig.Type = "oneshot";
          };
        })
        (builtins.attrNames cfg.containers)));

      networking = {
        nat = {
          enable = true;
          internalInterfaces = [ "ve-+" ];
        };
      };

      containers = (mapAttrs
        (c: a:
          let
            # NOTE(Dylan Green): Setup various sockets under /run/user/1000
            link-user = pkgs.writeShellScriptBin "link-user" ''
              ${lib.optionalString a.audio.pipewire.enable "ln -s /srv/run/user/1000/pipewire-0 /run/user/1000/pipewire-0"}
              ${a.extra.link-user-extras}
            '';

            allowedDevicesPre = lib.lists.optionals a.gpu.accel [
              {
                node = "char-drm";
                modifier = "rwm";
              }
              {
                node = "/dev/dri/*";
                modifier = "rw";
              }
            ] ++ lib.lists.optionals a.inputs.enable [
              {
                node = "char-input";
                modifier = "rwm";
              }
              {
                node = "char-hidraw";
                modifier = "rwm";
              }
              {
                node = "/dev/uinput";
                modifier = "rw";
              }
            ] ++ lib.lists.optionals a.gpu.nvidia [
              {
                node = "/dev/nvidia0";
                modifier = "rw";
              }
              {
                node = "/dev/nvidiactl";
                modifier = "rw";
              }
              {
                node = "/dev/nvidia-uvm";
                modifier = "rw";
              }
              {
                node = "/dev/nvidia-uvm-tools";
                modifier = "rw";
              }
              {
                node = "/dev/nvidia-modeset";
                modifier = "rw";
              }
            ];

            # NOTE(Dylan Green): Networking setup
            disableNet = v: if a.network.disable == true then " " else v;
            disableNetBool = v: if a.network.disable == true then true else v;
          in
          {

            # NOTE(Dylan Green): Passthrough Syscalls
            extraFlags = lib.concatLists [
              (map (z: "--system-call-filter=${z}") a.extra.syscalls)
              (map (z: "--drop-capability=${z}") a.extra.drop-caps)
              (map (z: if builtins.elem z (a.extra.drop-caps) then "" else "--capability=${z}") a.extra.keep-caps)
            ];

            interfaces = mkIf a.isolate.networking-to-container a.isolate.interfaces;

            ephemeral = true;
            autoStart = true;

            privateNetwork = disableNetBool a.network.private;
            localAddress = disableNet a.network.localAddress;
            hostAddress = disableNet a.network.hostAddress;

            bindMounts = lib.optionalAttrs (a.audio.pipewire.enable)
              {
                "/srv/run/user/1000/pipewire-0" = {
                  hostPath = a.audio.pipewire.socket;
                  isReadOnly = false;
                };

                "/srv/run/user/1000/pulse/native" = {
                  hostPath = a.audio.pipewire.pulse-socket;
                  isReadOnly = false;
                };

                "/yacc" = {
                  hostPath = "/yacc/${c}";
                  isReadOnly = false;
                };

                "/tmp/container-manager-bouncer.sock" = {
                  hostPath = "/tmp/container-manager-bouncer.sock";
                  isReadOnly = false;
                };
              } // lib.optionalAttrs (a.audio.pulseaudio.enable) {
                "/srv/run/user/1000/pulse" = {
                  hostPath = a.audio.pulseaudio.socket;
                  isReadOnly = false;
                };

              } // lib.optionalAttrs (a.dbus.share) {
              "/srv/run/user/1000/bus" = {
                hostPath = "/tmp/dbus-${c}/dbus-proxy";
                isReadOnly = false;
              };
            } // lib.optionalAttrs (a.inputs.enable) {
              # NOTE(Dylan Green): Probably insecure, ymmv
              "/dev/input" = {
                hostPath = "/dev/input";
                isReadOnly = false;
              };

              "/dev/uinput" = {
                hostPath = "/dev/uinput";
                isReadOnly = false;
              };
            } // lib.optionalAttrs (a.gpu.accel) {
              "/dev/dri" = {
                hostPath = "/dev/dri";
                isReadOnly = false;
              };
            } // lib.optionalAttrs (a.gpu.wayland.enable) {
              "/srv/run/user/1000/wayland-0" = {
                hostPath = a.gpu.wayland.socket;
                isReadOnly = false;
              };
            } // lib.optionalAttrs (a.extra.home-manager-setup) {
              "/nix/var/nix/profiles/per-user/container" = {
                hostPath = "/tmp/${c}/per-user-profile";
                isReadOnly = false;
              };

              "/nix/var/nix/gcroots/per-user/container" = {
                hostPath = "/tmp/${c}/per-user-gcroots";
                isReadOnly = false;
              };

              "/nix/var/nix/profiles/per-user/root".isReadOnly = true;
            } //
            (builtins.listToAttrs
              (map
                (t: {
                  name = t.container-directory;
                  value = {
                    hostPath = t.host-directory;
                    isReadOnly = t.readonly;
                  };
                })
                (a.extra.bindMounts))) // (builtins.listToAttrs (map
              (h: {
                name = "/home/${h.container-user}/${h.path}";
                value = {
                  hostPath = if !(isNull h.rebind-host-path)
                    then "/home/${h.user}/${h.rebind-host-path}"
                    else "/home/${h.user}/${h.path}";
                  isReadOnly = h.readOnly;
                };
              })
              (a.extra.home-binds)));


            allowedDevices = allowedDevicesPre ++ a.extra.allowedDevices;

            config = (
              {
                imports = [
                  (a.extra.systemConfig)
                ];

                nixpkgs.config.allowUnfree = true;

                systemd.services = {
                  "home-manager-setup" = {
                    enable = a.extra.home-manager-setup;
                    wantedBy = [ "home-manager-container.service" ];
                    script = ''
                      chown -R container /nix/var/nix/{profiles,gcroots}/per-user/container
                    '';
                  };

                  "container-manager" = {
                    enable = true;
                    wantedBy = [ "multi-user.target" ];
                    path = [ pkgs.util-linux ];
                    serviceConfig = {
                      Type = "simple";
                      ExecStart = "${pkgs.container-manager}/bin/client";
                    };
                  };

                  "fix-permissions" = {
                    wantedBy = [ "multi-user.target" ];
                    serviceConfig = {
                      Type = "oneshot";
                    };
                    script = ''
                      mkdir -p /home/container/.local
                      mkdir -p /home/container/.cache
                      mkdir -p /home/container/.config
                      chown -R container:users /home/container/.local
                      chown -R container:users /home/container/.cache
                      chown -R container:users /home/container/.config
                    '';
                  };
                };

                system.stateVersion = a.stateVersion;
                systemd.user.services = {
                  linkuser = {
                    after = [ "default.target" ];
                    wantedBy = [ "default.target" ];
                    script = "${link-user}/bin/link-user";
                  };
                };

                hardware.opengl = mkIf (a.gpu.accel) {
                  enable = true;
                  driSupport = true;
                  driSupport32Bit = true;
                };

                sound = mkIf a.audio.pipewire.enable {
                  enable = true;
                  extraConfig = mkIf a.audio.pipewire.setup-alsa ''
                    pcm_type.pipewire {
                      libs.native = ${a.pkgs.pipewire.lib}/lib/alsa-lib/libasound_module_pcm_pipewire.so ;
                      libs.32Bit = ${a.pkgs.pkgsi686Linux.pipewire.lib}/lib/alsa-lib/libasound_module_pcm_pipewire.so ;
                    }
                    ctl_type.pipewire {
                      libs.native = ${a.pkgs.pipewire.lib}/lib/alsa-lib/libasound_module_ctl_pipewire.so ;
                      libs.32Bit = ${a.pkgs.pkgsi686Linux.pipewire.lib}/lib/alsa-lib/libasound_module_ctl_pipewire.so ;
                    }
                    pcm.!default {
                      type pipewire
                    }
                    ctl.!default {
                      type pipewire
                    }
                  '';
                };
                networking = {
                  hostName = c;
                  networkmanager.enable = a.network.private;
                };
                environment = {
                  systemPackages = [ container-shim ] ++ (map (g: g.package) a.apps)
                  ++ lib.optionals a.gpu.accel (with a.pkgs; [
                    hicolor-icon-theme
                    gnome.adwaita-icon-theme
                  ]);

                  variables = {
                    NIX_PATH = lib.mkForce "nixpkgs=${a.pkgs.path}";
                  } // lib.optionalAttrs (a.gpu.nvidia) {
                    # Use EGL because we can't truly access the kernel
                    NVD_BACKEND = "egl";
                    LIBVA_DRIVER_NAME = "nvidia";
                    GBM_BACKEND = "nvidia-drm";
                    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
                  } // lib.optionalAttrs (a.gpu.wayland.enable) {
                    WAYLAND_DISPLAY = "/srv/run/user/1000/wayland-0";
                  } // lib.optionalAttrs (a.gpu.x11.enable) {
                    DISPLAY = a.gpu.x11.display-var;
                  } // lib.optionalAttrs (a.audio.pipewire.enable || a.audio.pulseaudio.enable) {
                    PULSE_SERVER = "/srv/run/user/1000/pulse/native";
                  } // lib.optionalAttrs (a.dbus.share) {
                    DBUS_SESSION_BUS_ADDRESS = "unix:path=/srv/run/user/1000/bus";
                  };

                  etc = {
                    "alsa/conf.d/50-pipewire.conf".source = "${a.pkgs.pipewire}/share/alsa/alsa.conf.d/50-pipewire.conf";
                    "alsa/conf.d/99-pipewire-default.conf".source = "${a.pkgs.pipewire}/share/alsa/alsa.conf.d/99-pipewire-default.conf";
                  };
                };
                users.mutableUsers = false;
                users.allowNoPasswordLogin = true;
                users.users."container" = {
                  isNormalUser = true;
                  extraGroups = [ "users" ]
                    ++ lib.optional a.gpu.accel "video"
                    ++ a.containerUser.extraGroups;
                  shell = a.containerUser.shell;
                  uid = a.containerUser.uid;
                };
              } // lib.optionalAttrs (a.gpu.nvidia) {
                hardware.nvidia.package = config.hardware.nvidia.package;
                services.xserver.videoDrivers = [ "nvidia" ];
                services.xserver.enable = true;
                services.xserver.displayManager.lightdm.enable = lib.mkForce false;
              }
            );
          })
        cfg.containers);
    };
}
