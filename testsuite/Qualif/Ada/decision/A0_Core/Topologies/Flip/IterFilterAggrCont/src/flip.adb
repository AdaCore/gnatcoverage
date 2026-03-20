pragma Ada_2022;

with Bool_Helpers; use Bool_Helpers;

package body Flip is

   function F (X : Boolean) return Boolean is
      Set : constant Bool_Set := [X];  -- # decl
      Res : constant Bool_Set := [for B of Set when not B => B]; -- # eval :o/d:
   begin
      return not Res.Is_Empty;  -- # returnVal
   end F;
end Flip;
