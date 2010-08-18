package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
      Values : array (Boolean) of Boolean := (False => False, True => True);
   begin
      return Values (A or else B); -- # evaluate
   end;
end;

