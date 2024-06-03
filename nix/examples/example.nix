{ config, pkgs, lib, pluto, ... }: {
  environment.etc."container-manager.config".text = builtins.toJSON {
    containerConfigs = {
      Discord = {
        "_filter_udev_events" = false;
        "_udev_filters" = [];
        "_inotify_watch" = {
          "/tmp/.X11-unix" = "Absolute";
        };
        "_automount" = {
          "/home/testuser/Pictures" = "Absolute";
          "/tmp/.X11-unix/X0" = "Absolute";
          "/dev/nvidia0" = "Absolute";
          "/dev/nvidiactl" = "Absolute";
          "/dev/nvidia-uvm" = "Absolute";
          "/dev/nvidia-uvm-tools" = "Absolute";
          "/dev/nvidia-modeset" = "Absolute";
        };
      };
    };
  };

  container-jails = {
    enable = true;
    containers = {
      "Discord" = {
        extra = {
          home-binds = [
            { path = ".config/discord"; type = "directory"; user = "testuser"; }
          ];
          systemConfig = {
            fonts.packages = with pkgs; [
              dejavu_fonts
              powerline-fonts
              source-code-pro
            ];
          };
        };

        network = {
          private = true;
          localAddress = "10.231.236.2";
          hostAddress = "10.231.236.1";
        };

        gpu = {
          accel = true;
          x11 = {
            enable = true;
            display-var = ":0";
          };
          wayland = {
            enable = true;
            socket = "/run/user/1000/wayland-1";
          };
          nvidia = true;
        };

        audio.pipewire = {
          enable = true;
        };

        apps = with pluto; [
          (desktopApp pkgs.discord)
        ];
      };
    };
  };
}
