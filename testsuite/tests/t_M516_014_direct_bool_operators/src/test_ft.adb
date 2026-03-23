with Assert;
with Ops; use Ops;

procedure Test_FT is
begin
   Eval (A => False, B => True);
   Assert (N_And = 0 and then N_Or = 1 and then N_Xor = 1);
end;

--# ops.adb
--  /eval-and/  s=>l+, d=>l! ## s=>0, d=>dT-
--  /and-true/  l- ## s-
--  /eval-or/   s=>l+, d=>l! ## s=>0, d=>dF-
--  /or-true/   l+ ## 0
--  /eval-xor/  s=>l+, d=>l! ## s=>0, d=>dF-
--  /xor-true/  l+ ## 0
