# `discard` #

A library implementing the [Carol programming language][1], providing
an eDSL for writing operations and a replica system that runs them.


## Quick start ##

First, get the project.

    $ git clone https://github.com/cuplv/card-systems
    $ cd card-systems

Next, build the project.  If you have the [`nix`](https://nixos.org)
package manager, the process is simple:

    $ cabal2nix ./. > package.nix
    $ nix-shell
    [nix-shell]$ cabal configure/build/run etc.

The current demo executable is a simple conflict-aware bank account,
in which replicas announce updates via http and store their history in
IPFS.  To try it, first make sure there is an [IPFS daemon][4] running
with it's API on port `5001` (the default).

    $ nix run nixpkgs.ipfs -c ipfs daemon --init

Then, open two terminals, and run the following:

    (term 1)$ cabal run discard-demo -- -c 2local.yaml -i alpha

    (term 2)$ cabal run discard-demo -- -c 2local.yaml -i beta

Now a node is running in each terminal.  Choose one and type `dp INT`
to add money to the account, or withdraw with `wd INT`.  You should
see the balance changes reflected in the other node's interface.

Try testing the safety of the system by running `wd INT` with enough
value to empty the account simultaneously on two nodes.

You can edit [`Main.hs`][2] to see how the library is used and make a
more interesting example.  The bank operations, written in the Carol
language, are defined in [`Bank.hs`][3].

Library documentation, which is still minimal and disorganized, can be
compiled with `cabal haddock`.

[1]: https://gateway.ipfs.io/ipfs/Qmf1H4ZmWbwYk6sAaK6RzZe3KsuQbx11KvzX5jaFPwsx8w/carol-lang-2018-12-14.pdf
[2]: ./demo/Main.hs
[3]: ./lib/Lang/Carol/Bank.hs
[4]: https://docs.ipfs.io/introduction/install/
