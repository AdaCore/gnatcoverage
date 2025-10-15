with Make_Calls;

--  Test the correctness of the function and call coverage indications for
--  overloaded operators when called.

procedure Test_Operators_Called is
begin
   Make_Calls;
end Test_Operators_Called;

--# make_calls.adb
-- /test/  l+ ## 0
-- /decl/  l+ ## 0
-- /op/    l+ ## 0
-- /short/ l! ## eT-,c-

--# operators.ads
-- /ok/    l+ ## 0
-- /fun/   l+ ## 0
-- /stmt/  l+ ## 0
