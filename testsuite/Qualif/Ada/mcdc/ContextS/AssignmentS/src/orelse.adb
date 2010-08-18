package body Orelse is

   procedure Eval_Or_Else (A, B : Boolean; E : out Boolean) is
   begin
      E := A or else B; -- # evaluate
   end;

   function Or_Else (A, B : Boolean) return Boolean is
      Value : Boolean;
   begin
      Eval_Or_Else (A, B, Value); -- # returnValue
      return Value;               -- # returnValue
   end;
end;

