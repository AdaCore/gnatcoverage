package body PandPor is
   function F (A, B, C : Boolean) return Boolean is
   begin
      return V : boolean := (A and then B) or else C do -- # evalStmt
        null; -- # returnValue
      end return;
   end;
end;

