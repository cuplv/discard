# `card-systems` #

A library and collection of runnable replica system examples, all
implementing the [*conflict-aware* replicated datatype][1]
programming/system model.


## Quick start ##

First, get the project.

    $ git clone https://github.com/cuplv/card-systems
    $ cd card-systems

Next, make sure you can build the project.  If you have the
[`nix`](https://nixos.org) package manager, here is a process:

    $ nix-env -i cabal2nix cabal-install
    $ cabal2nix --shell . > shell.nix
    $ nix-shell --command "cabal configure"
    $ cabal build

The current demo executable is very simple.  Edit
[`./cardr/Main.hs`][2] to see how the library is used and make a more
interesting example.


## Alternate setup ##
Without nix, or if nix doesn't work, here's another way to set up the system.

1. [Install ipfs](https://docs.ipfs.io/introduction/install/)
1. [Install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
1. Execute the following command within the repo:
    ```
    $ stack setup
    ```
1. To build and run the code, use the commands,
    ```
    $ stack build
    $ stack exec cardr
    ```

[1]: https://arxiv.org/abs/1802.08733
[2]: ./cardr/Main.hs
