package body Ornot is
   function F (A, B : Boolean) return Boolean is
   begin
      return Value : Boolean do -- # returnValue
        Value := A or else (not B);   -- # evalStmt
      end return;
   end;
end;

