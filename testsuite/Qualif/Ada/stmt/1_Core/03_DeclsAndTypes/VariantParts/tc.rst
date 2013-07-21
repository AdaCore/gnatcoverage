Variant Parts in record type declarations
=========================================

Check SC related to Variant Parts in record type declarations

The optional variant part of a record type declaration works like an implicit
case statement. The choice of a specific variant for an object triggers the
evaluation of the corresponding initialization expressions (if any), which,
via subprogram calls, might execute code that does not have an explicit
connection with the place of the object declaration.

Exercise variant record declarations for which distinct variants trigger
execution of distinct sequences of statements and check that only the code
associated to the chosen variants is reported as covered.

