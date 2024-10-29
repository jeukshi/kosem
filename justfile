_default:
  @just --list

# Build package.
build kosem-name='all':
  cabal build {{kosem-name}}

# Test package.
test kosem-name='all':
  cabal test {{kosem-name}} --test-show-details=direct

format:
    fourmolu -i kosem-postgresql

# run in ghcid
dev:
	ghcid --warnings --test "main"

run:
  cabal run

# find typos
typos:
  typos
