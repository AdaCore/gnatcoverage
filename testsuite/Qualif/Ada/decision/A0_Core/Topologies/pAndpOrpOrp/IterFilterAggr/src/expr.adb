pragma Ada_2022;

package body Expr is

   function F (A, B, C, D : Boolean) return Boolean is
      type Zero_Or_One is range 0 .. 1;  -- # decl
      type Bool_Arr is array (Zero_Or_One range <>) of Boolean; -- # decl
      subtype Singleton is Bool_Arr (1 .. 1);  -- # decl
      Arr : constant Singleton := [True];  -- # decl
      Res : constant Bool_Arr := [for Elt of Arr when (A and then B) or else (C or else D) => Elt];  -- # eval :o/d:
   begin
      return Res'Length = 1;  -- # retVal
   end;
end;
