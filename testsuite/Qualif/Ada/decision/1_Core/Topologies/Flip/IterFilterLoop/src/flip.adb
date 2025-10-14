pragma Ada_2022;
package body Flip is
   function F (X : Boolean) return Boolean is
      Arr : constant array (Positive range 1 .. 1) of Boolean := [X]; -- # decl
      Res : Boolean := False;                                         -- # decl
   begin
      for B of Arr when not B loop  -- # eval :o/d:
         Res := True;               -- # returnTrue
      end loop;
      return Res;               -- # returnVal
   end F;
end Flip;
