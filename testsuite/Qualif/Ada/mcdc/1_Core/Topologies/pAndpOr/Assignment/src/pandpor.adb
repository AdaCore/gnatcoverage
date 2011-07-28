package body PandPor is

   procedure Eval (A, B, C: Boolean; E : out Boolean) is
   begin
      E := (A and then B) or else C; -- # evalStmt :o/e:
   end;

   function F (A, B, C : Boolean) return Boolean is
      Value : Boolean;
   begin
      Eval (A, B, C, Value); -- # returnValue
      return Value;          -- # returnValue
   end;
end;

