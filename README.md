# libnix

Haskell bindings to the [nix package manager][nix]

This project aims to provide an interface to call [nix][nix] functionality from
the safe haven that is Haskell. As much type safety as possible should be
guaranteed to the user.

The project consists of two broad steps:

1. bindings to the command line tools
2. bindings to the nix libraries themselves

At the moment, an beta version of 1. is implemented, together with a small
number of tests to check for possible changes in the interface,
which consists mainly of three functions:

```haskell
parseNixExpr :: Text                 -> NixAction ParseError NixExpr
instantiate  :: NixExpr              -> NixAction InstantiateError (StorePath Derivation)
realize      :: StorePath Derivation -> NixAction RealizeError (StorePath Realized)
```

which do what you’d expect; and two helper functions

```haskell
eval             :: NixExpr -> NixAction InstantiateError ()
parseInstRealize :: Text    -> NixAction NixError (StorePath Realized)
```

where `parseInstRealize` performs all three steps at once.

[nix]: https://github.com/NixOS/nix


## Nix Prefetch Wrappers

We implement an additional module that creates nicely typed wrappers
for `nix-prefetch-X` tools, please see the module documentation what
is supported exactly.

```
url :: UrlOptions -> NixAction PrefetchError (Sha256, StorePath Realized)
git :: GitOptions -> NixAction PrefetchError GitOutput
```


## C++ bindings

The second steps would be to directly bind into the C++ library. That could
either mean writing a C wrapper and using Haskell’s native FFI, or generating
bindings with [Hoppy][hoppy], which we’d prefer. Hoppy would need to be able to
[handle C++ exceptions][exc] first, though.

[hoppy]: http://khumba.net/projects/hoppy/
[exc]: https://gitlab.com/khumba/hoppy/issues/10

## Other nix libraries on hackage

- [cabal2nix](https://hackage.haskell.org/package/cabal2nix): the executable at the base of nix haskell support
- [language-nix](https://hackage.haskell.org/package/language-nix): library cabal2nix uses to generate its nix files
- [hnix](https://hackage.haskell.org/package/hnix): a project to implement the nix expression language in haskell
- [nix-eval](https://hackage.haskell.org/package/nix-eval): runtime eval of Haskell code using nix
- [simple-nix](https://hackage.haskell.org/package/simple-nix): looks like a nix parser, but not entirely sure

It doesn’t look like the scope of libnix collides with any of these packages.
