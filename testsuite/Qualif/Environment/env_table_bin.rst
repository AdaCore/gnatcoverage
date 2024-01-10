.. csv-table::
   :header: "Item #"| "Description"| "Expected value"
   :widths: 5, 30, 60
   :delim:  |

   e1 | Host Operating System name and version | Linux Redhat 7
   e2 | GNATcoverage executable name and version | GNATcoverage 19.3 (20200318)
   e3 | GNAT Pro compiler executable name and version | gcc (GCC) 7.3.1 20180924 (for GNAT Pro 19lts 20200331) [i686-pc-linux-gnu]
   s1 | GNAT Pro compilation switches | -g -fpreserve-control-flow -fdump-scos -gnat12
   s2 | GNAT Pro Runtime Library Profile | No --RTS switch expected. Programs
   shall honor the :ref:`language-scope` restrictions, nevertheless.
