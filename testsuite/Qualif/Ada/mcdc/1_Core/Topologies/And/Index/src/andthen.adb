package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
      Values : array (Boolean) of Boolean := (False => False, True => True);
   begin
      return Values (A and then B); -- # evalStmt :o/e:
   end;
end;

