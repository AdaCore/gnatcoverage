Object Declarations involving heap or stack dynamic allocation
===============================================================

Check SC of Object Declarations involving heap or stack dynamic allocation

Check that an object declaration is reported as uncovered if and only if
it is not elaborated.
Exercise object declarations for which we expect to have execution
of code from explicit initialization expressions involving

* dynamic heap allocation for standard "new" allocators

* dynamic stack allocation for functions returning unconstrained objects

Check declarations in both global (i.e., library-level packages) and
local (e.g., blocks, subprograms) contexts.

