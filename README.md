log2sql
=======

Parses a log file and writes it to a [SQLite] database for better querying.

Usage
-----

`log2sql --help`

For example,

`log2sql -f myfile.log -d ";" colname1 colname2 colname3 colname4`

will parse the file `myfile.log` with four columns, separated by `;`s,
and write the output to `myfile-out.db`.

Likewise, you could do

`cat myfile.log | log2sql -d ";" colname1 colname2 colname3 colname4`

by leaving the `-f` flag off. This will write a database called
`log-data.db`.

`log2sql` will always merge superfluous colmuns into the last one.
For example, if you specify two columns but your data has three, then
`log2sql` will merge the third column into the second.


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

License
-------

Copyright &copy; Covenant Eyes 2014

This package is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php)
(see `LICENSE.txt`).
