pragma Ada_2012;
pragma Assertion_Policy (Post => Disable);

function Plus (A, B : Integer) return Integer
  with Post => (if (A > 0) then (B > 0) else (B <= 0)) -- # eval
is
begin
   return A + B; -- # stmt
end;
   


