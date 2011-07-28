package body Ornot is

   function F (A, B : Boolean) return Boolean is
      Values : array (Boolean) of Boolean := (False => False, True => True);
   begin
      return Values (A or else (not B)); -- # evalStmt :o/e:
   end;
end;

