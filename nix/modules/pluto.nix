{ config, pkgs, lib, ... }: {
  _module.args = {
    pluto = rec {
      directBind = x: { host-directory = x; container-directory = x; };
      directBindMulti = a: map (x: directBind x) a;

      writeHostUserBind = user: l: map (x: x // { user = user; }) l;
      rewriteContainerUserBind = user: l: map (x: x // { container-user = user; }) l;

      enableShim = { generateShim = true; };
      enableDesktop = { generateDesktopFile = true; };

      disableShim = { generateShim = false; };
      disableDesktop = { generateDesktopFile = false; };

      desktopApp = pkg: enableDesktop // { package = pkg; };
      cliApp = pkg: enableShim // { package = pkg; };

      withName = name: x: x // { desktopName = name; };
      withExeName = name: x: x // { exe-name = name; };

      generateShimsMulti = l: map (x: x // enableShim) l;
      generateDesktopMulti = l: map (x: x // enableDesktop) l;

      dontGenerateShimsMulti = l: map (x: x // disableShim) l;
      dontGenerateDesktopMulti = l: map (x: x // disableDesktop) l;

      rewriteContainerUser = user: {
        users.users."container" = {
          name = user;
          home = "/home/${user}";
        };
      };

      dbusDefaults = {
        talk = [
          "org.freedesktop.Notifications"
          "org.freedesktop.portal.Notifications"
        ];
        call = [
          { interface = "org.freedesktop.portal.Desktop"; rule = "*"; }
        ];
      };
    };
  };
}
