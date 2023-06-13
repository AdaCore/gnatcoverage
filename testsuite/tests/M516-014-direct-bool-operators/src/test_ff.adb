with Assert;
with Ops; use Ops;

procedure Test_FF is
begin
   Eval (A => False, B => False);
   Assert (N_And = 0 and then N_Or = 0 and then N_Xor = 0);
end;

--# ops.adb
--  /eval-and/  s=>l+, d=>l! ## s=>0, d=>dT-
--  /and-true/  l- ## s-
--  /eval-or/   s=>l+, d=>l! ## s=>0, d=>dT-
--  /or-true/   l- ## s-
--  /eval-xor/  s=>l+, d=>l! ## s=>0, d=>dT-
--  /xor-true/  l- ## s-

