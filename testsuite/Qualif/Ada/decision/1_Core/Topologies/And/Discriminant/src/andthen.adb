package body Andthen is

   type Expr (Value : Boolean) is null record;

   function And_Then (A, B : Boolean) return Boolean is
      E : Expr (Value => A and then B); -- # andthen :o/0:
   begin
      return E.Value;  -- # retVal
   end;
end;



