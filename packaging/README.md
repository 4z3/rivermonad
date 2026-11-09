# Distribution packages

| Distribution | File | Needs |
|---|---|---|
| Arch Linux | [`arch/PKGBUILD`](arch/PKGBUILD) | river ≥ 0.4 (`extra/river` is 0.5) |
| Fedora ≥ 44 | [`fedora/rivermonad.spec`](fedora/rivermonad.spec) | river ≥ 0.4 (Fedora 43 still has 0.3) |

Like xmonad, rivermonad compiles `~/.config/rivermonad/rivermonad.hs` with
ghc at run time, so the packages install the Haskell library too and
depend on ghc:

* **Arch** ships Haskell libraries as shared objects only. The PKGBUILD
  builds them that way and enables the `dynamic-config` cabal flag, which
  makes rivermonad compile the configuration with `-dynamic`.
* **Fedora** splits it like other Haskell packages: `rivermonad` (the
  binary and the session) requires `ghc-rivermonad-devel` (the library).

Both install the session: `/usr/bin/river-session`,
`/usr/share/wayland-sessions/rivermonad.desktop` ("river + rivermonad" on
the login screen) and `/usr/share/rivermonad/init`, which river-session
uses when there is no `~/.config/river/init`.

## Arch Linux

```sh
cd packaging/arch
makepkg -si
```

## Fedora

```sh
sudo dnf install rpm-build rpmdevtools dnf-plugins-core
sudo dnf builddep packaging/fedora/rivermonad.spec
spectool -g -R packaging/fedora/rivermonad.spec
rpmbuild -bb packaging/fedora/rivermonad.spec
sudo dnf install ~/rpmbuild/RPMS/*/{,ghc-}rivermonad*-0.1.0-*.rpm
```

The same spec builds in [COPR](https://copr.fedorainfracloud.org/).

## A new release

1. Bump `version:` in `rivermonad.cabal`, `pkgver` in the PKGBUILD and
   `Version:` in the spec (plus a `%changelog` entry).
2. Tag it: `git tag -a v0.2.0 -m "rivermonad 0.2.0" && git push --tags`.
3. Update `sha256sums` in the PKGBUILD with `updpkgsums`.
