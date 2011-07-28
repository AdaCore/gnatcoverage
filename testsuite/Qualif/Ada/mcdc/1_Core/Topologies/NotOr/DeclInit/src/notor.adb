package body Notor is

   function F (A, B : Boolean) return Boolean is
      E : boolean := (not A) or else B;  -- # evalStmt :o/e:
   begin
      return E;  -- # returnValue
   end;
end;



