package body Andthen is

   type Expr (A, B : Boolean) is record
      Value : Boolean := A and then B;  -- # andthen
   end record;

   function And_Then (A, B : Boolean) return Boolean is
      E : Expr (A, B);
   begin
      return E.Value;  -- # retVal
   end;
end;



