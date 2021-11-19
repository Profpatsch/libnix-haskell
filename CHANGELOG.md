# libnix

## 0.4.0.1 -- 2021-11-19

* Fix `Prefetch.url`. The constructed command was missing flags for `urlUnpack = True`.

## 0.4.0.0 -- 2021-11-19

* Allow setting a log function when running a nix action, which can log command invocations.
* Add an `m` attribute to `NixAction` to make it into a proper `MonadTrans`.
* Reexport `Types` from the other modules, so that explicit import is not required.

## 0.3.0.0 -- 2021-11-16

* Remove `Eq` instance from `NixExpr` (not sensible)
* Replace the `NixAction` tuple with a more semantic `NixActionError`
* Update to 2021 and remove dependency on protolude

## 0.2.0.0 -- 2018-08-12

* First released version.
