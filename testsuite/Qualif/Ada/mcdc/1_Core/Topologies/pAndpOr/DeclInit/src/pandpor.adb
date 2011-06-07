package body PandPor is

   function F (A, B, C : Boolean) return Boolean is
      Value : boolean := (A and then B) or else C; -- # evalStmt
   begin
      return Value;  -- # returnValue
   end;
end;
