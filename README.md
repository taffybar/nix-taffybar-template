# nix-taffybar-template

A minimal, project-style [taffybar](https://github.com/taffybar/taffybar) configuration template built with Nix flakes.

This is intended as a clean answer to [taffybar issue #563](https://github.com/taffybar/taffybar/issues/563): a fully working flake-based example that builds a custom `taffybar` binary from a normal Haskell project.

## Goals

- No submodules
- No machine-specific paths
- No user-specific naming
- Good-looking default bar with a small amount of CSS

## Quick Start

```bash
git clone https://github.com/<you>/nix-taffybar-template
cd nix-taffybar-template
nix run .#default
```

## Development

```bash
nix develop
cabal run taffybar
```

Or use `just` shortcuts:

```bash
just run
just dev-run
just restart
```

## Structure

- `flake.nix`: pins dependencies and exposes `packages.default`, `apps.default`, and `devShells.default`
- `nix-taffybar-template.cabal`: project-style executable definition
- `taffybar.hs`: widget/layout configuration
- `taffybar.css`: styling loaded from package data

## Customization

- Edit widget lists in `taffybar.hs` (`startWidgets`, `centerWidgets`, `endWidgets`)
- Adjust colors/spacing in `taffybar.css`
- Rename the package and executable once you fork
