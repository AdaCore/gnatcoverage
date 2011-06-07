package body Andthen is

   type Expr is record
      Value : Boolean;
   end record;

   function And_Then (A, B : Boolean) return Boolean is
      E : Expr := (Value => A and then B);  -- # evalStmt
   begin
      return E.Value;  -- # returnValue
   end;
end;



