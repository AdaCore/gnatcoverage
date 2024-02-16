package body Andthen is

   procedure Eval_And_Then (A, B : Boolean; E : out Boolean) is
   begin
      E := A and then B; -- # evalStmt :o/e:
   end;

   function And_Then (A, B : Boolean) return Boolean is
      Value : Boolean; -- # decl
   begin
      Eval_And_Then (A, B, Value); -- # returnValue
      return Value;                -- # returnValue
   end;
end;
