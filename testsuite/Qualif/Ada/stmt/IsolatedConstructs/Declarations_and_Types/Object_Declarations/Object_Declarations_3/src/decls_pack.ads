--  This package contains library-level record variable object declaration with
--  explicit initialization expression and subprograms containing local
--  record variable object declarations with explicit initialization that are
--  expected to be covered only when subprograms are called.

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   My_String : Var_String := (Len => 3, Data => "Ada");    -- # dcls

   procedure Local_Swap (C1, C2 : in out Coordinate);

   function Local_Fun (Arg : Var_String) return Var_String;
   --  For the Data field of the argument, replace each character with the
   --  next one.
end Decls_Pack;
