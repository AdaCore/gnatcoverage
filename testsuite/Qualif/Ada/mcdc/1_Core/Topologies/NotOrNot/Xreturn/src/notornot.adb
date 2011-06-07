package body Notornot is
   function F (A, B : Boolean) return Boolean is
   begin
      return Value : Boolean do -- # returnValue
        Value := (not A) or else (not B);   -- # evalStmt
      end return;
   end;
end;

