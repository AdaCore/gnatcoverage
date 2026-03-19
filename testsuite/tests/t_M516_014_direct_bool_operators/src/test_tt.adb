with Assert;
with Ops; use Ops;

procedure Test_TT is
begin
   Eval (A => True, B => True);
   Assert (N_And = 1 and then N_Or = 1 and then N_Xor = 0);
end;

--# ops.adb
--  /eval-and/  s=>l+, d=>l! ## s=>0, d=>dF-
--  /and-true/  l+ ## 0
--  /eval-or/   s=>l+, d=>l! ## s=>0, d=>dF-
--  /or-true/   l+ ## 0
--  /eval-xor/  s=>l+, d=>l! ## s=>0, d=>dT-
--  /xor-true/  l- ## s-
