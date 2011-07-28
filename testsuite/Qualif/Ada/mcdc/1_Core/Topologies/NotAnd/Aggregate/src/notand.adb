package body Notand is

   type Expr is record
      Value : Boolean;
   end record;

   function F (A, B : Boolean) return Boolean is
      E : Expr := (Value => (not A) and then B);  -- # evalStmt :o/e:
   begin
      return E.Value;  -- # returnValue
   end;
end;



