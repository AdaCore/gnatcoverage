package body PandPor is
   function F (A, B, C : Boolean) return Boolean is
   begin
      return V : boolean := (A and then B) or else C do -- # evalStmt :o/e:
        null; -- # returnValue
      end return;
   end;
end;

