--  This package contains library-level access variable object declaration
--  with explicit initialization expression and subprograms containing local
--  access variable object declarations with and without explicit
--  initialization that are expected to be covered only when subprograms are
--  called.

with Decls_Support; use Decls_Support;
with Support;       use Support;
package Decls_Pack is

   Access_All_Integer_Var : Access_All_Integer := Integer_Var'Access;-- # dcls

   I : Integer := Identity (1);                                      -- # dcls
   --  Needed to avoid creating a dummy xcov report

   procedure Local_Swap (V1, V2 : in out Access_Integer);

   function Local_Fun (I : Integer) return Access_Const_Integer;
   --  If I > 0 returns access value pointing to the value of I, otherwise
   --  returns null
end Decls_Pack;
