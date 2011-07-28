package body Andnot is
   function F (A, B : Boolean) return Boolean is
   begin
      return Value : boolean := A and then not B do   -- # evalStmt :o/e:
        null; -- # returnValue
      end return;
   end;
end;

