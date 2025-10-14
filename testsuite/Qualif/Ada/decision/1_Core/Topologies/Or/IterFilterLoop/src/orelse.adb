pragma Ada_2022;

package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
      Arr : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
      Res : Boolean := False;  -- # decl
   begin
      for Elt of Arr when A or else B loop  -- # orelse :o/d:
         Res := Elt;  -- # retTrue
      end loop;
      return Res;  -- # retVal
   end;
end;
