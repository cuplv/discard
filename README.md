# `card-systems` #

A library and collection of runnable replica system examples, all
implementing the [*conflict-aware* replicated datatype][1]
programming/system model.


## Quick start ##

Running `$ cabal run` will run the example replica network (a pair of
replicas concurrently communicating in a multi-threaded program; no
network involved).

The script that sets up the replicas and gives them their inputs is
the `test2Replicas` function in the [`cardr/Main.hs` file][2].


## Progress ##

Currently, a very basic conflict-*free* replica system has been
implemented, which uses an in-memory event graph store and thread
communication channels as a network.


## Next steps ##

- HTTP-based communication between replicas
  - Needs a general "communication pipe" abstraction
- IPFS-based event graph store
  - Implementing the [`EventGraph` typeclass][3]
- Operations with queries (advancing from a conflict-free to
  conflict-aware model)
- Embedded CARD programming language for operations
- Verification for CARD language


[1]: https://arxiv.org/abs/1802.08733
[2]: ./cardr/Main.hs
[3]: ./lib/Data/EventGraph.hs
