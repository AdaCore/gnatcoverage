pragma Ada_2022;

package body Expr is

   function F (A, B, C, D : Boolean) return Boolean is
      Arr : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
      Res : Boolean := False;  -- # decl
   begin
      for Elt of Arr when (A or else B) or else (C and then D) loop  -- # eval :o/d:
         Res := Elt;  -- # retTrue
      end loop;
      return Res;  -- # retVal
   end;
end;
