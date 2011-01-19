package body Notor is
   function F (A, B : Boolean) return Boolean is
   begin
      return Value : Boolean do -- # returnValue
        Value := (not A) or else B;   -- # evalStmt
      end return;
   end;
end;

