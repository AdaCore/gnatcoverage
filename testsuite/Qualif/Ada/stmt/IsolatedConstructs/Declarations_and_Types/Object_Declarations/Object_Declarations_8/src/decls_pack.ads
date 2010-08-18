--  This package contains an object of a derived record type declaration with
--  no explicit initialization, but the parent record type does have default
--  initialization expressions for components and subprograms containing local
--  declarations of similar objects that are expected to be covered only when
--  subprograms are called
with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   Derived_Discrete_Coordinate_V : Derived_Discrete_Coordinate; -- # dcls

   procedure Local_1 (Res : in out Boolean);
   --  Straightline subprogram with "# code1" marks on statements and "# decl1"
   --  marks on object declarations. Changes its parameter to the opposite
   --  value

   function Local_2 (Arg : Boolean) return Boolean;
   --  Straightline subprogram with "# code2" marks on statements and "# decl2"
   --  marks on object declarations. Returns the value opposite to its
   --  parameter.
end Decls_Pack;
