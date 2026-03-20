with Assert;
with Values; use Values;

procedure Test_1 is
   B : Bool;
begin
   Equal (X => 1, Y => 2, B => B);
   Assert (B.Value = False);
   Equal2 (X => 1, Y => 1, Z => 4, T => 8, B => B);
   Assert (B.Value = False);
end;

--# values.adb
--  /eq/      l+ ## 0
--  /andthen/ d=>l+, mu=>l! ## d=>0, mu=>eT-
