package body AndPorP is

   function F (A, B, C : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;         -- # decl
      Values : Bmap := (False => False, True => True); -- # decl
   begin
      return Values (A and then (B or else C)); -- # evalStmt :o/e:
   end;
end;

