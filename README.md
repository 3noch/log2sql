log2sql
=======

Parses a log file and writes it to a [SQLite] database for better querying

Usage
-----

Currently `log2sql` only supports [decrypted] Windows client log files.

`log2sql myfile.log` will create `test.db` with your log's data as columns of the table `test`.


Building
--------

You must have

  * [Glasgow Haskell Compiler](http://www.haskell.org/ghc/)
  * [Cabal](http://www.haskell.org/cabal/) (which comes with the [Haskell Platform](http://www.haskell.org/platform/))

Then you can

```bash
$ cd log2sql
$ cabal install
```
