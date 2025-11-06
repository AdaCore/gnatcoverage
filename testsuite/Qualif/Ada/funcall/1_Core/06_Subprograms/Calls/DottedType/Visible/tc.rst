**Check FCC for calls to functions declared in nested packages**

Declare types and functions returning those types in nested packages. Make
calls to these functions and check the coverage information. All return types
are visible from the calls sites, and call and function coverage should be
reported normally.
