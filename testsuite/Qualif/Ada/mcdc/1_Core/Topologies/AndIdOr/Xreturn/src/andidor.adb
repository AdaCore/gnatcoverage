
package body Andidor is
   function F (A, B, C : Boolean) return Boolean is
   begin
      return V : Boolean :=                  -- # returnValue
        A and then Identity (B or else C) do -- # evalStmt :o/e:
        null; -- # returnValue
      end return;
   end;
end;

