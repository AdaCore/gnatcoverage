--  This package contains library-level declaration of an array object with
--  explicit index constraint (and without explicit initialization expression)
--  and subprograms containing local object declarations (of integer and array
--  types, both with explicit constraints and without any explicit or implicit
--  initialization) that are expected to be covered only when subprograms are
--  called

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   Small_Vector : Vector (1 .. Identity (2)); -- # dcls

   procedure Local_1
     (Res : in out Boolean;
      X, L, R : in Integer);
   --  Straightline subprogram with "# code1" marks on statements and "# decl1"
   --  marks on object declarations. It sets the value of Res parameter to
   --  (X in L .. R).

   function Local_2
     (Arg : Boolean;
      X, L, R : Integer)
     return Boolean;
   --  Straightline subprogram with "# code2" marks on statements and "# decl2"
   --  marks on object declarations. Returns the value equal to (X in L .. R).
end Decls_Pack;
