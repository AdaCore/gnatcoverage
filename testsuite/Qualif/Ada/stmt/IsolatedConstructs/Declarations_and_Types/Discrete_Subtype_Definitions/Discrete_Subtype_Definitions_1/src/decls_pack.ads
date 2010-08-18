--  This package contains library-level array type declaration with discrete
--  subtype definition that contains an explicit constraint and subprograms
--  declarations with similar array type declarations that are expected to be
--  covered only when subprograms are called

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   type Global_Arr is array (Index range Identity (1) .. 10) of Week_Day; -- # dcls

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
