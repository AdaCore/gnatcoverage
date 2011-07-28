package body Notornot is

   function F (A, B : Boolean) return Boolean is
      Values : array (Boolean) of Boolean := (False => False, True => True);
   begin
      return Values ((not A) or else (not B)); -- # evalStmt :o/e:
   end;
end;

