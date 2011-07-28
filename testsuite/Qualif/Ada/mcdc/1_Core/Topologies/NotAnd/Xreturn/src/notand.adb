package body Notand is
   function F (A, B : Boolean) return Boolean is
   begin
      return Value : boolean := (not A) and then B do   -- # evalStmt :o/e:
        null; -- # returnValue
      end return;
   end;
end;

