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

