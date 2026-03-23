pragma Ada_2022;

package body Andnot is

   function F (A, B : Boolean) return Boolean is
      Items : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
      Res   : Boolean := False;  -- # decl
   begin
      for Elt of Items when A and then not B loop  -- # evalStmt :o/d:
         Res := Elt;  -- # decisionTrue
      end loop;
      return Res;  -- # returnValue
   end;
end;
