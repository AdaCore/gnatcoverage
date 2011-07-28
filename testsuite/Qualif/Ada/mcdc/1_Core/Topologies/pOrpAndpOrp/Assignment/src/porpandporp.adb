package body PorPandPorP is

   procedure Eval_And_Then (A, B, C, D : Boolean; E : out Boolean) is
   begin
      E := (A or else B) and then (C or else D); -- # evalStmt :o/e:
   end;

   function F (A, B, C, D : Boolean) return Boolean is
      Value : Boolean;
   begin
      Eval_And_Then (A, B, C, D, Value); -- # returnValue
      return Value;                -- # returnValue
   end;
end;

