pragma Ada_2012;
pragma Assertion_Policy (Pre => Disable);

function Plus (A, B : Integer) return Integer
  with Pre => (if (A > 0) then (B > 0) else (B <= 0)) -- # eval
is
begin
   return A + B; -- # stmt
end;
   


