pragma Ada_2022;

package body PorPandPorP is

   function F (A, B, C, D : Boolean) return Boolean is
      Items : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
      Res   : Boolean := False;  -- # decl
   begin
      for Elt of Items when (A or else B) and then (C or else D) loop  -- # evalStmt :o/d:
         Res := Elt;  -- # decisionTrue
      end loop;
      return Res;  -- # returnValue
   end;
end;
