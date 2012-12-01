package body Orelse is

   procedure Eval_Or_Else (A, B : Boolean; E : out Boolean) is
   begin
      E := A or else B; -- # evalStmt :o/e:
   end;

   function Or_Else (A, B : Boolean) return Boolean is
      Value : Boolean; -- # decl
   begin
      Eval_Or_Else (A, B, Value); -- # returnValue
      return Value;               -- # returnValue
   end;
end;

