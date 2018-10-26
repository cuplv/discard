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

The current demo executable is a simple conflict-aware bank account,
in which replicas announce updates via http and store their history in
IPFS.  To try it, first make sure there is an [IPFS daemon][4] running
with it's API on port `5001` (the default).

    $ ipfs daemon --init

Then, open three terminals, and run the following:

    (term 1)$ cabal run -- \
        0 http://localhost:45550/ \
        1 http://localhost:45551/ \
        2 45552

    (term 2)$ cabal run -- \
        1 http://localhost:45551/ \
        2 http://localhost:45552/ \
        0 45550

    (term 3)$ cabal run -- \
        2 http://localhost:45552/ \
        0 http://localhost:45550/ \
        1 45551

Now a node is running in each terminal.  Choose one and type `check`
to see the current shared account value (0 to start).  Add money with
`dp INT`, withdraw with `wd INT`.

You may notice that `wd INT` does not return immediately; it is
waiting for confirmation from the other nodes that they will not
concurrently withdraw.  The other nodes are waiting for your input and
thus "not listening".  Wake them up by running `check` in both of
their terminals.  The first should now complete the withdrawal.

You can also run `check exact` to see an absolutely up-to-date account
value.  This must perform coordination similar to that of `wd INT`.

You can edit [`./cardr/Main.hs`][2] to see how the library is used and
make a more interesting example.  The bank operations are implemented
in [`./lib/CARD/LQ/Bank.hs`][3].


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
[3]: ./lib/CARD/LQ/Bank.hs
[4]: https://docs.ipfs.io/introduction/install/
