package body Andthen is
   function And_Then (A, B : Boolean) return Boolean is
   begin
      return A and then B;  -- # andthen
   end;
end;

