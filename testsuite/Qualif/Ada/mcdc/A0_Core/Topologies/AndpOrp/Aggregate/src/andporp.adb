package body AndPorP is

   type Expr is record
      Value : Boolean;
   end record;

   function F (A, B, C : Boolean) return Boolean is
      E : Expr := (Value => (A and then (B or else C))); -- # evalStmt :o/e:
   begin
      return E.Value;  -- # returnValue
   end;
end;
