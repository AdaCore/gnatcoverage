package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
      E : Boolean := A and then B;  -- # andthen :o/0:
   begin
      return E;  -- # retVal
   end;
end;



