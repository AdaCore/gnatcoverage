package body AndPorP is

   procedure Eval_And_Then (A, B, C : Boolean; E : out Boolean) is
   begin
      E := A and then (B or else C); -- # evaluate
   end;

   function F (A, B, C : Boolean) return Boolean is
      Value : Boolean;
   begin
      Eval_And_Then (A, B, C, Value); -- # returnValue
      return Value;                -- # returnValue
   end;
end;

