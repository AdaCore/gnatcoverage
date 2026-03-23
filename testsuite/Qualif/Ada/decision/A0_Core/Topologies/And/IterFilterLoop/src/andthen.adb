pragma Ada_2022;

package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
      Arr : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
      Res : Boolean := False;  -- # decl
   begin
      for Elt of Arr when A and then B loop  -- # andthen :o/d:
         Res := Elt;  -- # retTrue
      end loop;
      return Res;  -- # retVal
   end;
end;
