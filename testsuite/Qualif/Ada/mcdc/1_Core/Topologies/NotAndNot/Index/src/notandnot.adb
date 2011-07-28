package body Notandnot is

   function F (A, B : Boolean) return Boolean is
      Values : array (Boolean) of Boolean := (False => False, True => True);
   begin
      return Values ((not A) and then (not B)); -- # evalStmt :o/e:
   end;
end;

