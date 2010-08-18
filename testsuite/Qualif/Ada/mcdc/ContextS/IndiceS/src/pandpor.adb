package body PandPor is
   function F (A, B, C : Boolean) return Boolean is
      Values : array (Boolean) of Boolean := (False => False, True => True);
   begin
      return Values ((A and then B) or else C); -- # evaluate
   end;
end;
