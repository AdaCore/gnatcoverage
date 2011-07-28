package body Notornot is

   type Expr is record
      Value : Boolean;
   end record;

   function F (A, B : Boolean) return Boolean is
      E : Expr := (Value => (not A) or else (not B));  -- # evalStmt :o/e:
   begin
      return E.Value;  -- # returnValue
   end;
end;



