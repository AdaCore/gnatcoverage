--  This package contains library-level constant declaration of a private
--  type and subprograms containing local private type object declarations
--  with and without explicit initialization that are expected to be covered
--  only when subprograms are called. The full declaration of a private type is
--  a record declaration with default initialization expression for components

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   T_Private_V : constant T_Private := T_Private_Zero;    -- # dcls

   procedure Local_1 (Res : in out Boolean);
   --  Straightline subprogram with "# code1" marks on statements and "# decl1"
   --  marks on object declarations. Changes its parameter to the opposite
   --  value

   function Local_2 (Arg : Boolean) return Boolean;
   --  Straightline subprogram with "# code2" marks on statements and "# decl2"
   --  marks on object declarations. Returns the value opposite to its
   --  parameter.
end Decls_Pack;
