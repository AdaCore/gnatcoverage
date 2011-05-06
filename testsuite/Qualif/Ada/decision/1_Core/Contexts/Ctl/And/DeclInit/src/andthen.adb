package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
      E : Boolean := A and then B;  -- # andthen
   begin
      return E;  -- # retVal
   end;
end;



