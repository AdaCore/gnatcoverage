pragma Ada_2022;

with Support; use Support;

package body AndIdOr is

   function F (A, B, C : Boolean) return Boolean is
      Items : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl
      Res   : Boolean := False;  -- # decl
   begin
      for Elt of Items when A and then Identity (B or else C) loop  -- # evalStmt :o/d:
         Res := Elt;  -- # decisionTrue
      end loop;
      return Res;  -- # returnValue
   end;
end;
