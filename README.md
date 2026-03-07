# Nestix

Nestix is a **structural formatter for Nix**. It rewrites *structure* (currently focused on attribute sets) rather than acting as a full pretty-printer.

It is designed to be used alongside a conventional Nix formatter such as **alejandra** or **nixfmt**:

1. Run alejandra or nixfmt to standardize whitespace and line breaking.
2. Run Nestix to normalize and (where appropriate) flatten attribute-set structure.

## Quickstart

Run Nestix after your usual Nix formatter. Example (single file):

```bash
alejandra file.nix # or nixfmt
nestix file.nix
```

Format a repository (recursively formats `*.nix` files in place):

```bash
nestix .
```

Check mode (exits with code `2` if changes would be made):

```bash
nestix --check .
```

Read from stdin and write to stdout:

```bash
cat file.nix | nestix
```

## What it changes

Nestix focuses on transformations that conventional formatters typically do not perform:

- Flattening small attribute-set subtrees into dotted attribute assignments (when it improves readability).
- Normalizing indentation when Nestix needs to move code.

Example (flattening an attribute set):

```nix
# Before
{
  attr1 = {
    attr2 = {
      attr3 = true;
    };
    attr4 = true;
  };
}

# After
{
  attr1.attr2.attr3 = true;
  attr1.attr4 = true;
}
```

Nestix does *not* blindly flatten everything. It keeps (or expands) nesting when flattening would reduce readability.

## What it does not try to do

- Replace alejandra or nixfmt: Nestix does not aim to make every Nix expression "pretty" on its own.
- Impose a style guide: Nestix is unopinionated and attempts to preserve the existing style (alignment, indentation, etc).

## Installation

### Nix (flakes)

Run without installing:

```bash
nix run github:Noah765/nestix
```

Install into your profile:

```bash
nix profile install github:Noah765/nestix
```

### Cargo (from Git)

```bash
cargo install --git https://github.com/Noah765/nestix
```

### NixOS (flake)

<details>
<summary>Minimal NixOS system flake snippet</summary>

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nestix = {
    url = "github:Noah765/nestix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    nixpkgs,
    nestix,
    ...
  }: let
    system = "x86_64-linux";
  in {
    nixosConfigurations.example = nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        {
          environment.systemPackages = [nestix.packages.${system}.default];
        }
      ];
    };
  };
}
```

</details>

## Usage

```text
nestix [OPTIONS] [INCLUDE]...
```

- If no `INCLUDE` paths are provided, Nestix reads Nix code from stdin.
- If paths are provided, Nestix traverses them recursively and formats `*.nix` files in place.

Common flags:

- `--check`: Do not write changes; print files that need formatting; exit with code `2` if any do.
- `--exclude <PATH>`: Skip a path (may be repeated).

See `nestix --help` for the full CLI help text.

## Performance

Nestix is intended to be fast enough to run over large file trees. As a rough datapoint, `nestix /path/to/nixpkgs` takes about **8 seconds** on an AMD Ryzen 5 3600.

## Contributing / Development

- Nix dev shell: `nix develop`
- Build: `cargo build --release`
- Test: `cargo test`

If you encounter a bug or would like a new formatting rule, please open an issue. Small, focused, reproducible snippets are extremely helpful.
