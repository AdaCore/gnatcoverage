package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
      E : boolean := A or else B;  -- # evalStmt
   begin
      return E;  -- # returnValue
   end;
end;



