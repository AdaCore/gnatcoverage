:option:`--output-dir` :
   Request that the report files (index and annotated sources for the
   ``xcov``, ``html`` and ``dhtml`` output formats) be output in the provided
   directory. If not specified, the default is the root project's object
   directory if using projects, and the current directory if not. The
   directory must exist prior to invoking |gcv|.

:option:`--report-title` :
   Request that generated HTML documents (index and annotated sources for the
   ``html`` and ``dhtml`` output formats) are assigned a customized title. For
   instance, passing ``--report-title="Project ABC"`` will yield titles such
   as: ``Project ABC - GNATcoverage Report``. If passed multiple times, passing
   an empty string last will restore the default behavior. This option is
   ignored is the selected output format does not support titles.

:option:`-T`, :option:`--trace` |marg|, |rarg| :
   Provide the set of execution traces for which a report is to be
   produced. When multiple traces are provided, |gcv| produces a consolidated
   result, as described in detail in the :ref:`consolidation` chapter of this
   manual.

:option:`--exec`:
   Override executable from traces. Trace files contain an indication of the
   executable used to generate them. This option causes the named executable
   to be loaded for coverage analysis, and to override the indication contained
   in any trace specified after it on the command line. An empty executable
   name may be specified to restore the default behaviour of using the
   indication contained in each trace file. Note that :option:`--exec` may
   appear last on the command line, in which case it applies to no trace file,
   but still causes the indicated executable to be included in the coverage
   analysis. This ensures that any code in that executable that is not exercised
   by some trace file will be reported as not covered.

