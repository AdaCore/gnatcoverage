--  This package contains library-level private variable object declaration
--  with explicit initialization expression and subprograms containing local
--  private variable object declarations with and without explicit
--  initialization that are expected to be covered only when subprograms are
--  called.

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   T_Private_V : constant T_Private := T_Private_Zero;    -- # dcls

   procedure Local_Swap (V1, V2 : in out T_Private);

   function Local_Fun (Arg : T_Private) return T_Private;
   --  Some meaningless computations...
end Decls_Pack;
