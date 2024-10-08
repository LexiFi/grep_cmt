> NOTE: this code is made available for the interest of the community, but is
> provided with absolutely no WARRANTY or SUPPORT of any kind. If you have
> questions about the code, feel free to contact us, but we may not be able to
> help you.  Suggestions for improvements are also likely to be
> ignored. Instead, we encourage you to take the approach (and the code) and
> develop it into a proper, user-friendly, powerful, code refactoring tool.
>
> Thanks! - LexiFi

# Structural search of OCaml code

`grep_cmt` is a command-line tool written at LexiFi to perform structural search
of OCaml code.

The code is released as-is in case it is of interest to the community. The
present code is extracted from a version used internally at LexiFi and we do not
pretend that it is ready for public consumption, but we are making the code
available to publicize the approach.

## Compilation

```sh
dune build
```

The tool requires OCaml 5.2 (since it depends on `compiler-libs`, compilation
aginst other versions of OCaml will require some adjustments).

This produces an executable `_build/install/bin/grep_cmt.exe` which can be run
from the command line.

## Usage

```shell
grep_cmt PATTERN
```

The tool assumes that it is executed within a Dune workspace, as it has a number
of heuristics that will not work otherwise. The tool will search the files under
the current directory subtree.

You will need to run `dune build @check` in your project to ensure all `.cmt`
files are up-to-date before using `grep_cmt`.

`PATTERN`: The pattern to search for. This should be a valid OCaml *expression*.

- The wildcard `__` matches any expression or any record field.

- A numbered wildcard `__1`, `__2`, ... matches any expression or record field
  and enforce strict equality of all the matching occurrences for the same
  number.

- An identifier (value or class) is matched as a suffix of the fully qualified
  path in the typed expression.

- Labels and constructors identifiers are matched as a suffix of the identifier
  in the typed expression.

- For function applications, it is allowed to omit in the pattern any argument
  of the actual function call; special forms are recognized to enforce that a
  given optional argument present or missing:
  ```
  foo ?arg:PRESENT
  foo ?arg:MISSING
  ```

- An expression `(... : typexpr)` matches any expression matching `...` and
  whose type is equal to `typexpr`.

- For `try..with`/`match..with`/`functions` expressions, the order of clauses
  doesn't matter.  A single clause of the searched expression can match several
  clauses of the code.  Same set-semantics for record expressions.

- An expression `e1.lid1` matches `e2.lid2 <- e3` if `e1` matches `e2` and the
  label `lid1` matches `lid2`.

- The expression `__.id` matches any *pattern* of the form `{...; P.id; ...}`
  for any prefix `P`. This rule was added so that grepping for `__.foo` will
  return every "get" and "set" of the record field `foo`, including reads in
  patterns.

## Examples

```sh
grep_cmt 'List.filter'
grep_cmt '(__ (__ : floatarray): float array)'
grep_cmt  'List.rev __ @ __'
grep_cmt 'match __ with None -> __ | Some __1 -> Some __1'
grep_cmt 'List.fold_left __ __ (List.map __ __)'
grep_cmt 'Stdlib.max (__:float) __'
```
