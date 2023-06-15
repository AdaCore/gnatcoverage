with Support; use Support;
with Div; use Div;

procedure Test_Divide_Nok is
   Result: Integer;
begin
   Divide (10,0, Result);

   Assert (N_Comp_Success = 0);
   Assert (N_Excpt_Prop = 1);
   Assert (N_Wrong_Excpt_Prop = 0);

end Test_Divide_Nok;

--# div.adb
-- /division/       l+ ## 0
-- /no_exception/   l- ## s-
-- /propagated_up/  l+ ## 0
-- /wrong_excpt/    l- ## s-
