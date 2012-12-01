package body Andthen is
   function And_Then (A, B : Boolean) return Boolean is
      E : Boolean; -- # decl
   begin
      E := A and then B;  -- # andthen :o/0:
      return E;           -- # retVal
   end;
end;

