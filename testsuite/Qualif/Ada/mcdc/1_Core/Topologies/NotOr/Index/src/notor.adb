package body Notor is

   function F (A, B : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;         -- # decl
      Values : Bmap := (False => False, True => True); -- # decl
   begin
      return Values ((not A) or else B); -- # evalStmt :o/e:
   end;
end;

