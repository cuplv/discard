# `card-systems` #

A library and collection of runnable replica system examples, all
implementing the [*conflict-aware* replicated datatype][1]
programming/system model.


## Quick start ##

The current example network is a single node using IPFS to store its
event graph.  To test it, set up an [ipfs daemon][4] with its API
exposed on port 5001 (the default) and then run the `cardr`
executable.

    $ ipfs init
    $ ipfs daemon --offline &
    $ cabal run cardr

The script that sets up the replica is the `testIpfsReplica` function
in the [`cardr/Main.hs` file][2].


## Progress ##

A full conflict-*free* replica system has been implemented, which uses
an in-memory event graph store and thread communication channels as a
network.

Additionally, an IPFS event graph backend has been implemented.


## Next steps ##

- Safe summarization of event histories
- Audit coordination protocol and associated interface
- IPFS pub-sub based communication
- Faster IPFS event graph backend, using direct manipulation of the
  DAG instead of reading/writing the unix filesystem
- HTTP based communication
- Operations with queries (advancing from a conflict-free to
  conflict-aware model)
- Embedded CARD programming language for operations
- Verification for CARD language


[1]: https://arxiv.org/abs/1802.08733
[2]: ./cardr/Main.hs
[3]: ./lib/Data/EventGraph.hs
[4]: https://github.com/ipfs/go-ipfs
