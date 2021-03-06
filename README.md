`diff-du` is a command line tool for tracking down where your disk space is
going by comparing two `du` runs.  Features:

- Output is sorted by diff size.  **Shows the worst offenders first.**
- Takes an optional size threshold, below which diffs are not output.
  **Only shows big differences.**
- Once a diff is output, it is not counted for its parents.  **Shows the
  real culprits.**  (See the example below.)
- Children of new or removed directories are ignored.  **New and removed
  directories are treated as a unit.**
- Output is in unified diff format.  Programs that do syntax highlighting
  should detect it automatically; try piping to `vi -`.  **Familiar, pretty
  output.**
- Accepts gzipped `du` output files.  **Supports archived du reports.**
- Runs `du` for you if given a directory.  **Compares live directories (or a
  live directory with a saved report).**

Here is an example (these files exist in the distribution):

`test/example.1.du`:

    5       /home/andrew/mail
    10      /home/andrew/my-proj/src
    10      /home/andrew/my-proj
    10      /home/andrew/old-proj
    25      /home/andrew

`test/example.2.du`:

    10      /home/andrew/mail
    20      /home/andrew/my-proj/src
    25      /home/andrew/my-proj
    10      /home/andrew/new-proj/src
    20      /home/andrew/new-proj
    60      /home/andrew

`diff-du --threshold 10 test/example.1.du test/example.2.du`:

    --- test/example.1.du
    +++ test/example.2.du
    +20      /home/andrew/new-proj
    +15      /home/andrew
    +10      /home/andrew/my-proj/src
    -10      /home/andrew/old-proj

Notes:

- Output is ordered, with the largest diff first, and negative diffs at the
  end.
- The `+5` diff for `/home/andrew/mail` is not output because it is below
  the threshold.
- Children of `/home/andrew/new-proj` are not output.
- `/home/andrew/my-proj` is not output, because a diff of `+10` has already
  been counted for `/home/andrew/my-proj/src`, and the remaining `+5` is
  below the threshold.
- The `+15` diff for `/home/andrew` represents the part of the actual `+35`
  diff that is not output for its children.

The last two points are potentially confusing, but in my opinion this the
most useful for focusing on the real changes.

Usage
=====

    Usage: diff-du [--threshold N] [--prune PATH]... PATH PATH
    PATH is either
    - a directory to run du on OR
    - a file (possibly gzipped) containing du output
      -h       --help          print this message
      -t N     --threshold=N   ignore differences below this threshold
      -p NAME  --prune=NAME    ignore entries below directiory NAME (eg. .git)
               --du-prog=PROG  use PROG as du
               --du-arg=ARG    pass ARG on to du

Cookbook
========

Sometimes you may want to ignore diffs under certain directories.  For
example, the objects in a `.git` directory are low-level details, and you
probably only care about the size of the whole thing.  This is what the
`--prune` option is for.  If you add `--prune .git` to your command line,
`diff-du` treats `.git` directories as a unit.  Their contents count towards
the size of `.git`, but are never displayed.  (This is different from the
`--exclude` option of `du`, which does not count excluded objects at all.)
The `--prune` option may be given multiple times.  Only exact matching of a
single path component is currently supported.

Building
========

Run `make`.  Requires the [Glasgow Haskell Compiler][ghc].  The executable
will be called `diff-du`.  There is no install target, so just copy it to
its final location yourself.

[ghc]: http://haskell.org/ghc

Hacking
=======

`diff-du` is written in pretty straight-forward Haskell.  If you don't know
much Haskell, please ask and I will try to help.  It would also be easy to
port to another language.

TODO
====

I would love suggestions for making `diff-du` more useful and intuitive.
You can report bugs and requests as GitHub issues, or send me email.

Related Work
============

I was surprised that not much effort has gone into addressing this problem.
There is a [Stack Overflow thread][stack] from 2009 asking about it, with
only one little script offered as an answer (until I posted this).  Lluís
Batlle i Rossell has written a simple version called [`du-diff`][du-diff].
That's all I found (as of April, 2013).

[stack]: http://stackoverflow.com/questions/1533593/is-there-a-standard-way-to-diff-du-outputs-to-detect-where-disk-space-usage-has
[du-diff]: http://vicerveza.homeunix.net/~viric/soft/du-diff/

License
=======

`diff-du` is licensed under the three-clause BSD license.  See the `LICENSE`
file.

Author
======

Andrew Pimlott, andrew@pimlott.net
