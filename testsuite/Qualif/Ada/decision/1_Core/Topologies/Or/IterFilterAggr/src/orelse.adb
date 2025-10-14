pragma Ada_2022;

package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
      type Zero_Or_One is range 0 .. 1;  -- # decl
      type Bool_Arr is array (Zero_Or_One range <>) of Boolean; -- # decl
      subtype Singleton is Bool_Arr (1 .. 1);  -- # decl
      Arr : constant Singleton := [True];  -- # decl
      Res : constant Bool_Arr := [for Elt of Arr when A or else B => Elt];  -- # orelse :o/d:
   begin
      return Res'Length = 1;  -- # retVal
   end;
end;
