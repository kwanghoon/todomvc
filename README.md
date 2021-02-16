# A PolyRPC Runtime in Haskell (with A TodoMVC web program)

## Getting started

To get started quickly building this TodoMVC web program with the PolyRPC runtime in Haskell, you need one thing to use [`Miso`](https://github.com/dmjio/miso) framework for the client and the other to use [`Scotty`](https://github.com/scotty-web/scotty) framekwork for the server.

For the client side, we recommend the [`nix`](https://nixos.org/nix) package manager with miso's binary cache provided by [`cachix`](https://miso-haskell.cachix.org/).

For the server side, you need [`stack`](https://www.haskellstack.org).

```
git clone https://github.com/kwanghoon/todomvc
cd todomvc/webserver
stack build
cd ../webclient
nix-build
```

To run the server, in todomvc/webserver,

```
stack exec -- webserver-exe todomvc
```

To run the client, in todomvc/webclient,

```
xdg-open ./result/bin/app.jsexe/index.html
```

Note my environment:
```
$ lsb_release -a 
No LSB modules are available.
Distributor ID:	Ubuntu
Description:	Ubuntu 20.10
Release:	20.10
Codename:	groovy

$ uname -a
Linux khchoi-ThinkPad-X1-Carbon-5th 5.8.0-43-generic #49-Ubuntu SMP Fri Feb 5 03:01:28 UTC 2021 x86_64 x86_64 x86_64 GNU/Linux
```

Our TodoMVC program shows the same client UI as in [the other TodoMVC programs](https://todomvc.com) but it has a server program with a list of todo items. The client UI interacts with the server to read and modify the list.

- <img src="https://github.com/kwanghoon/MySmallBasic/blob/master/todomvc/doc/todomvc_running.png"/>

## Links

- [The polyrpc compilation system](https://github.com/kwanghoon/polyrpc) being developed by Kwanghoon Choi
- [The experimental Scala-based PolyRPC runtime](https://github.com/tzbob/rrpc) where TodoMVC written in PolyRPC was developed by Bob Reynders.



