package body Notor is

   function F (A, B : Boolean) return Boolean is
      E : boolean := (not A) or else B;  -- # evalStmt
   begin
      return E;  -- # returnValue
   end;
end;



