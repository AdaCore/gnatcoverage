package body Andthen is

   type Expr (AA, BB : Boolean) is record
      Value : Boolean := AA and then BB;  -- # andthen
   end record;

   function And_Then (A, B : Boolean) return Boolean is
      E : Expr (A, B);  -- # retVal
   begin
      return E.Value;   -- # retVal
   end;
end;
