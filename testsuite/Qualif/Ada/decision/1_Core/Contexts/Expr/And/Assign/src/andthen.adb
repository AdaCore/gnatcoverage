package body Andthen is
   function And_Then (A, B : Boolean) return Boolean is
      E : Boolean;
   begin
      E := A and then B;  -- # andthen
      return E;           -- # retVal
   end;
end;

