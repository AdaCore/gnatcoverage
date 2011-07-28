package body Notor is

   type Expr is record
      Value : Boolean;
   end record;

   function F (A, B : Boolean) return Boolean is
      E : Expr := (Value => (not A) or else B);  -- # evalStmt :o/e:
   begin
      return E.Value;  -- # returnValue
   end;
end;



