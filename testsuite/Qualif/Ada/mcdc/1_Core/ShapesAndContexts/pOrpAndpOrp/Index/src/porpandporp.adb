package body PorPandPorP is

   function F (A, B, C, D : Boolean) return Boolean is
      Values : array (Boolean) of Boolean := (False => False, True => True);
   begin
      return Values ((A or else B) and then (C or else D)); -- # evalStmt
   end;
end;

