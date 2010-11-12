package body AndIdOr is
   function F (A, B, C : Boolean) return Boolean is
      Value : array (Boolean) of Boolean := (False => False, True => True);
   begin
      return Value (A and then Identity (B or else C)); -- # evalStmt
   end;
end;
