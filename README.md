# Container Manager

A runtime-agnostic linux daemon to facilitate bind mounts and dynamic-bind mounts that rely on iNotify along with forwarding system events from `udev` to be acted upon by the host and client daemon

## Example Configuration
```json
{
  "containerConfigs": {
    "myContainer": {
     "_udev_filters": [],
     "_automount": {
       "/dev/nvidia-modeset": "Absolute",
       "/dev/nvidia-uvm": "Absolute",
       "/dev/nvidia-uvm-tools": "Absolute",
       "/dev/nvidia0": "Absolute",
       "/dev/nvidiactl": "Absolute",
       "/tmp/.X11-Unix/X0": "Absolute"
     },
     "_filter_udev_events": false,
     "_inotify_watch": {
       "/tmp/.X11-unix": "Absolute",
     }
    }
  }
}
```

in the configuration above we bind mount multiple `nvidia` device nodes into the container at start-time,
alongside this we also bind in our `X11` socket and tell the `inotify` watcher to rebind the socket if it is created or removed from that directory

## How To Run
To run the container-manager host you can issue `<path_to_bindir>/server` which will read the config file from `/etc/container-manager.conf`

The client gets it's configured state declared by the server, so the client needs no config files. to run the client issue `<path_to_bindir>/client`

The cli tool has documentation in `--help` on the cli, it is available at `<path_to_bindir>/container-util`

## Caveats
- Currently the host daemon expects a `/yacc` directory to exist to manage bind mounts
  - The server will create sub-directories under `/yacc` to facilitate per-container bind mounts (ex: `/yacc/myContainer`)
  - The server will also create a `udev` subdirectory (ex: `/yacc/myContainer/udev`) to forward bind mounts caused by udev events
- Currently the client daemon expects a `/yacc` directory to exist to manage bind mounts
- Currently the cli utility expects `/tmp/container-manager-cli.sock` to exist outside of the container

## TODO
- [ ] Switch to a better event system (maybe reflex? or a custom file-descriptor based event-system)
- [ ] Wire up command running support to create desktop files to run applications inside the container
- [ ] Hot-reloading of container configurations
- [ ] Use `Data.Binary` instead of `Data.Aeson` to encode and decode data (maybe use a custom serializer here)
- [ ] Server: Cli argument to server to change config file location
- [ ] Cli: Support more state configuration commands, along with running commands
- [ ] Spawn our own lightweight namespace containers and install ourselves into the container to manage orchestration
- [ ] Make everything more configurable
