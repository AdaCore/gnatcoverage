with Make_Calls;

--  Test the correctness of the function and call coverage indications for
--  overloaded operators when not called.

procedure Test_Operators_Not_Called is
begin
   null;
end Test_Operators_Not_Called;

--# make_calls.adb
-- /test/  l- ## f-
-- /decl/  l- ## s-
-- /op_v/  l- ## s=>s-,f=>s-,c-
-- /op_A/  l- ## s=>s-,f=>s-,c-,c-,c-
-- /op_B/  l- ## s=>s-,f=>s-,c-,c-
-- /op_C/  l- ## s=>s-,f=>s-,c-,c-
-- /op_D/  l- ## s=>s-,f=>s-,c-,c-
-- /op_E/  l- ## s=>s-,f=>s-,c-,c-
-- /op_F/  l- ## s=>s-,f=>s-,c-,c-,c-
-- /short/ l- ## s=>s-,f=>s-,c-,c-

--# operators.ads
-- /ok/    l+ ## 0
-- /fun/   l- ## f-
-- /stmt/  l- ## s-
