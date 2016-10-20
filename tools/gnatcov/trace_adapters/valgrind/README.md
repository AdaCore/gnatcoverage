This directory holds the Valgrind trace adapter for GNATcoverage.

This adapter consists in pieces allowing the construction of an instrumented
version of Valgrind, able to produce (upon request) execution traces suitable
for analysis by `gnatcov coverage`.

In order to build it:

* Make sure you have Valgrind 3.7.0 installed (with headers):

    ```
    <prefix>/bin/valgrind
    <prefix>/lib/valgrind/
    <prefix>/include/valgrind/
    ```

* Build and install:

    ```shell
    $ make
    $ sudo make install
    ```
