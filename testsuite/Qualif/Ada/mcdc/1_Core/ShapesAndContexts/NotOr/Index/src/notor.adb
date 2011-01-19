package body Notor is

   function F (A, B : Boolean) return Boolean is
      Values : array (Boolean) of Boolean := (False => False, True => True);
   begin
      return Values ((not A) or else B); -- # evalStmt
   end;
end;

