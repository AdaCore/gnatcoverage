package body Notandnot is
   function F (A, B : Boolean) return Boolean is
   begin
      return Value : boolean := (not A) and then (not B) do   -- # evalStmt
        null; -- # returnValue
      end return;
   end;
end;

