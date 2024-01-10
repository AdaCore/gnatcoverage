.. csv-table::
   :header: "Item #"| "Description"| "Expected value"
   :widths: 5, 30, 60
   :delim:  |

   e1 | Host Operating System name and version | Windows x86_64
   e2 | GNATcoverage version | GNATcoverage 23.1 (20230113)
   e3 | GNAT Pro GCC version | GCC 11.3.1 20220922 (for GNAT Pro 23.1 20230113)
   e4 | gprbuild version | GPRBUILD Pro 23.1 (20230113) (x86_64-w64-mingw32)
   s1 | GNAT Pro compilation switches | -gnat12
   s2 | GNAT Pro Runtime Library Profile | No --RTS switch expected. Programs shall nevertheless honor the :ref:`language-scope` restrictions.
   s3 | GNATcov dump trigger switch value | :literal:`--dump-triger=atexit`
   s4 | GNATcov dump channel switch value | :literal:`--dump-channel=bin-file`
