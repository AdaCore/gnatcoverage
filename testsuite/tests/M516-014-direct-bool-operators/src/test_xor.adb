with Assert;
with Ops; use Ops;

procedure Test_Xor is
begin
   Eval (A => False, B => False);
   Eval (A => True, B => False);
   Assert (N_And = 0 and then N_Or = 1 and then N_Xor = 1);
end;

--# ops.adb
--  /eval-and/  s=>l+, d=>l! ## s=>0, d=>dT-
--  /and-true/  l- ## s-
--  /eval-or/   l+ ## 0
--  /or-true/   l+ ## 0
--  /eval-xor/  l+ ## 0
--  /xor-true/  l+ ## 0

