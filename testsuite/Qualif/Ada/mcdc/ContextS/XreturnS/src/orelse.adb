package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
   begin
      return Value : Boolean do -- # returnValue
        Value := A or else B;   -- # evaluate
      end return;
   end;
end;

