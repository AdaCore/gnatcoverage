with Support; use Support;
with Div; use Div;

procedure Test_Divide_Nok is

   Result : Integer;

begin

   Divide (12, 0, Result);

   Assert (N_In_MyCE_Handler = 1);
   Assert (N_In_Wrong_Handler = 0);

end Test_Divide_Nok;

--# div.adb
-- /explicit_check/ l+ ## 0
-- /explicit_raise/ l+ ## 0
-- /computation/    l- ## s-
-- /explicit_handler/ l+ ## 0
-- /wrong_handler/  l- ## s-
