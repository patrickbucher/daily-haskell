# OpenBSD Setup

## Native Setup

OpenBSD provides packages for Haskell (`ghc`, `ghci`) and Cabal (`cabal`) as ports.

Install the Glasgow Haskell Compiler on OpenBSD (`ghc`, `ghci`):

```sh
doas pkg_add ghc
```

Install the Cabal Install tool `cabal`:

```sh
doas pkg_add cabal-install
```

Uninstall both:

```sh
doas pkg_remove ghc cabal-install
```

Fetch package list from Hackage:

```sh
cabal update
```

Install the Stack build tool:

```sh
cabal install stack
```

