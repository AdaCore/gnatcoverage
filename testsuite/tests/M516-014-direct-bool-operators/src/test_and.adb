with Assert;
with Ops; use Ops;

procedure Test_And is
begin
   Eval (A => True, B => True);
   Eval (A => True, B => False);
   Assert (N_And = 1 and then N_Or = 2 and then N_Xor = 1);
end;

--# ops.adb
--  /eval-and/  l+ ## 0
--  /and-true/  l+ ## 0
--  /eval-or/   s=>l+, d=>l! ## s=>0, d=>dF-
--  /or-true/   l+ ## 0
--  /eval-xor/  l+ ## 0
--  /xor-true/  l+ ## 0

