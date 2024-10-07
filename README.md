# Structural search of OCaml values

`grep_cmt` is a command-line tool written in OCaml, designed to perform a structural search within OCaml `.cmt` files (compiled module types). This tool leverages OCaml's abstract syntax to allow advanced pattern matching and structural queries on OCaml projects.

## Features

- **Wildcard Matching**: Supports wildcards for expressions and record fields with `__` matching any expression or field, and numbered wildcards `__1`, `__2`, ... ensuring equality among occurrences.

- **Pattern Matching**: Grep patterns can include identifiers, labels, constructors, function applications with optional arguments, and more.

- **Structural Constraints**: Allows for type constraints in expressions and is flexible about the order of clauses in match expressions.

- **Compatibility Modes**: Offers options for Emacs-compatible output and verbose logging.

## Usage

```shell
grep_cmt [OPTIONS] PATTERN
```

- `PATTERN`: The string or pattern to search for. This should be a valid OCaml expression pattern.

### Options

- `-verbose` or `-v`: Enable verbose mode for detailed logging.
- `-root`: Set the root directory for the search to start from.
- `-emacs`: Produce output friendly for Emacs compilation mode.
- `-i`: Perform a case-insensitive search.
- `-I <dir>`: Include a directory in the load path for the search.


## Installation

```sh
dune build
```

This produces an executable `grep_cmt.exe` which can be run from the command line.

You'll need to run `dune build @check` in your project to ensure all `.cmt` files are up-to-date before using `grep_cmt`.

## Examples

```sh
grep_cmt 'List.filter'
grep_cmt '(__ (__ : floatarray): float array)'
grep_cmt  'List.rev __ @ __'
grep_cmt 'match __ with None -> __ | Some __1 -> Some __1'
grep_cmt 'List.fold_left __ __ (List.map __ __)'
grep_cmt 'Stdlib.max (__:float) __'
```
