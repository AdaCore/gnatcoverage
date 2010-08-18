--  This package contains library-level variable scalar object declaration with
--  explicit initialization expression and subprograms containing local
--  variable scalar object declarations with explicit initializations that are
--  expected to be covered only when subprograms are called

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   Count : Natural := Identity (0);                -- # dcls

   procedure Local_1 (Res : in out Boolean);
   --  Straightline subprogram with "# code1" marks on statements and "# decl1"
   --  marks on object declarations. Changes its parameter to the opposite
   --  value

   function Local_2 (Arg : Boolean) return Boolean;
   --  Straightline subprogram with "# code2" marks on statements and "# decl2"
   --  marks on object declarations. Returns the value opposite to its
   --  parameter.
end Decls_Pack;
