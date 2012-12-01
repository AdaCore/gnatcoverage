package body Notand is

   function F (A, B : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean; -- # decl
      Values : constant Bmap := (False => False, True => True); -- # decl
   begin
      return Values ((not A) and then B); -- # evalStmt :o/e:
   end;
end;

