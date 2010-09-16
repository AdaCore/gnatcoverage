--  This package contains library-level array variable object declaration with
--  explicit initialization expression and subprograms containing local
--  array variable object declarations with explicit initialization that are
--  expected to be covered only when subprograms are called.

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   Matrix_C : constant Matrix (1 .. 2, 1 .. 2) := ((1, 2), (3, Identity (4))); -- # dcls

   procedure Local_Swap (M1, M2 : in out Matrix);

   function Local_Fun (Arg : Matrix) return Matrix;
   --  Changes the sign of each component of the argument.
end Decls_Pack;
