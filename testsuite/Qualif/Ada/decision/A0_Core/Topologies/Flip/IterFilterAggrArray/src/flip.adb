pragma Ada_2022;
package body Flip is
   function F (X : Boolean) return Boolean is
      type Zero_Or_One is range 0 .. 1;  -- # decl
      type Bool_Arr is array (Zero_Or_One range <>) of Boolean;  -- # decl
      subtype Singleton is Bool_Arr (1 .. 1);  -- # decl
      Arr : constant Singleton := [X];  -- # decl
      Res : constant Bool_Arr := [for B of Arr when not B => B];  -- # eval :o/d:
   begin
      return Res'Length = 1;  -- # returnVal
   end F;
end Flip;
