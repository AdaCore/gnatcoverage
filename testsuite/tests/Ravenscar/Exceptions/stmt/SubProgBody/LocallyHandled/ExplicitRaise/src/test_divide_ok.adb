with Support; use Support;
with Div; use Div;

procedure Test_Divide_Ok is

   Result : Integer;

begin

   Divide (12, 2, Result);

   Assert (N_In_MyCE_Handler = 0);
   Assert (N_In_Wrong_Handler = 0);

end Test_Divide_Ok;

--# div.adb
-- /explicit_check/ l+ ## 0
-- /explicit_raise/ l- ## s-
-- /computation/    l+ ## 0
-- /explicit_handler/ l- ## s-
-- /wrong_handler/  l- ## s-
