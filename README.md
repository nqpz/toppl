# Top Prolog, an aggressive Prolog compiler

[![Build Status](https://travis-ci.org/nqpz/toppl.svg?branch=master)](https://travis-ci.org/nqpz/toppl)

Top Prolog, or toppl for short, is a compiler for (a subset of) Prolog.
Toppl strives for subjective elegance and is not efficient, but is easy
to write backends for.  Key points:

  - Breadth-first search.
  - Transformation to an intermediate representation with tail recursion
    as the only form of recursion.
  - Very simple final intermediate representation.
  - Generally not fast.
  - Memory-hungry.


## Installation

You need [Stack](https://docs.haskellstack.org/en/stable/README/) to
install toppl.  Then run `stack install` at the root of this repository.


## Usage

Once installed, run just `toppl` to get a list of commands, and `toppl
<command>` to get the usage for that command.

Here are some things you might want to try out:

### Interpreting a program

```
echo 'human_path(head, hat, Path).' | toppl interpret tests/path.pl
```

This will print three answers.


### Compiling a program and running it

First run:

```
toppl c tests/path.pl
```

This will generate the file `tests/path.c` and compile it to
`tests/path` with gcc.

Then run:

```
echo 'human_path(head, hat, Path).' | tests/path -b
```

This will print the same answers you get when interpreting the program
(possibly with a different indentation).  You can also time it:

```
echo 'human_path(head, hat, Path).' | tests/path -b -t
```


### Compiling a program with debugging abilities

You can compile a Prolog program to let it print extensive debug
information during its run.  Run:

```
toppl c -d tests/path.pl
```

Then run:

```
echo 'human_path(head, hat, Path).' | tests/path -b
```

This will print a lot of information of what is going on, in particular
*all* attempted unifications.  If you want slightly less debugging
information, you can run it with a lower debug level:

```
echo 'human_path(head, hat, Path).' | tests/path -b -d 1
```

Note that the runtime `-d` flag is only available when a program is
compiled with the `-d` flag of `toppl c`.


### An adversarial program

Take a look at `tests-temp/binarith.pl`.  This program implements binary
arithmetic, but the interesting part (from the perspective of toppl) is
that the generated code has an explosion in the amount of rules when
going through the compiler passes, due to the overly simplifying nature
of the compiler (a design choice).  The resulting program does partially
work, but eats up large amounts of memory and is very slow.


## Design decisions

Here is a small overview of the compiler passes and their purposes:

TODO


## License

Toppl is free software under the terms of the GNU General Public
License version 3 (or any later version).  Copyright (C) 2019 Niels
G. W. Serup.
