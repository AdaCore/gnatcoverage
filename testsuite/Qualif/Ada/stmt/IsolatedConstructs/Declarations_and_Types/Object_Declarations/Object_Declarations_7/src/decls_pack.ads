--  This package contains library-level access object declaration with no
--  explicit initialization and subprograms containing local access object
--  declarations with no explicit initialization that are expected to be
--  covered only when subprograms are called

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   Access_All_Integer_Var : Access_All_Integer := Integer_Var'Access;-- # dcls

   procedure Local_1 (Res : in out Boolean);
   --  Straightline subprogram with "# code1" marks on statements and "# decl1"
   --  marks on object declarations. Changes its parameter to the opposite
   --  value

   function Local_2 (Arg : Boolean) return Boolean;
   --  Straightline subprogram with "# code2" marks on statements and "# decl2"
   --  marks on object declarations. Returns the value opposite to its
   --  parameter.
end Decls_Pack;
