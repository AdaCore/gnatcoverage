**Exercise SC on object declarations involving static or fixed-sized stack allocation**

Check that such an object declaration is reported as uncovered if and only if
it is not elaborated.
Exercise object declarations for which we expect to have implicit
execution of code for the initializations:

* Object declarations with explicit initialization expressions (scalar,
  access, or composite values of record or array type);

* Declarations of objects whose types define implicit initialization (access
  types and record types with component initializers).

Check local and global declarations. Check declarations that allow static or
fixed-size stack allocations only, which do not need a secondary stack
(e.g. from functions returning unconstrained objects) or dynamic memory
allocation (e.g. from standard "new" allocators).

LRMREF: 3.3.1, 3.6
