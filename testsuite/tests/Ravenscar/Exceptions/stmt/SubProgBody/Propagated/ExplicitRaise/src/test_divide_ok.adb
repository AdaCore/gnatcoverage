with Support; use Support;
with Div; use Div;

procedure Test_Divide_Ok is
   Result: Integer;
begin
   Divide (10,5, Result);

   Assert (N_Comp_Success = 1);
   Assert (N_Excpt_Prop = 0);
   Assert (N_Wrong_Excpt_Prop = 0);

end Test_Divide_Ok;

--# div.adb
-- /division/       l+ ## 0
-- /no_exception/   l+ ## 0
-- /propagated_up/  l- ## s-
-- /wrong_excpt/    l- ## s-
-- /explicit_check/ l+ ## 0
-- /explicit_raise/ l- ## s-
-- /computation/    l+ ## 0
