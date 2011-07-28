package body AndPorP is

   function F (A, B, C : Boolean) return Boolean is
      Value : boolean := A and then (B or else C); -- # evalStmt :o/e:
   begin
      return Value;  -- # returnValue
   end;
end;
