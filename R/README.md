Limitations
===========

Apart from examples in different languages the R version needs installation of
packages. I haven't found a way to use namespaces in R without building *and*
installing the package. I was quite surprised that the namespace / package
or even module concept (as it is there in Python, Perl, Lisp, Haskell) does
not work with R without installing a package.

The packages can be installed locally without special rights, use the scripts
install.sh or install.cmd (latter not yet begun).

The config files would look different in a real R world and R is not the language
of choice for the given problem. However, to keep implementations compatible,
the config files are read in a more manual style, supporting all features
of the other versions.

FIXME:
* Add test cases to check paths for confg file reader
* Add test cases to check strip
* Add test cases for getopt
* Add support for config file reader to read from memory
