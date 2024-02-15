with Assert;
with Ops; use Ops;

procedure Test_Or is
begin
   Eval (A => False, B => False);
   Eval (A => True, B => True);
   Assert (N_And = 1 and then N_Or = 1 and then N_Xor = 0);
end;

--# ops.adb
--  /eval-and/  l+ ## 0
--  /and-true/  l+ ## 0
--  /eval-or/   l+ ## 0
--  /or-true/   l+ ## 0
--  /eval-xor/  s=>l+, d=>l! ## s=>0, d=>dT-
--  /xor-true/  l- ## s-
