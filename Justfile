set shell := ["bash", "-euo", "pipefail", "-c"]

run:
  nix run .#default

build:
  nix build .#default

dev-shell:
  nix develop

dev-run:
  nix develop -c cabal run taffybar

restart:
  pkill -x taffybar || true
  nohup nix run .#default >/tmp/nix-taffybar-template.log 2>&1 &
  echo "taffybar restarted; logs: /tmp/nix-taffybar-template.log"
