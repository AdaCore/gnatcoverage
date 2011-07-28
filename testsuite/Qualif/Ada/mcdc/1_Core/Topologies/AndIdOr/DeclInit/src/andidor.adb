package body AndIdOr is

   function F (A, B, C : Boolean) return Boolean is
      R : boolean := A and then Identity (B or else C); -- # evalStmt :o/e:
   begin
      return R;  -- # returnValue
   end;
end;

