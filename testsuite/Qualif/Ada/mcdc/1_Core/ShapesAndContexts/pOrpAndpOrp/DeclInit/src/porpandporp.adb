package body PorPandPorP is

   function F (A, B, C, D : Boolean) return Boolean is
      Value : boolean := (A or else B) and then (C or else D); -- # evalStmt
   begin
      return Value;  -- # returnValue
   end;

end;
