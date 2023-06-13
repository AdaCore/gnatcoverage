with Support; use Support;
with Div; use Div;

procedure Test_Divide_Nok is

   Result : Integer;

begin

   Divide (12, 0, Result);

   Assert (N_In_Handler = 1);

end Test_Divide_Nok;

--# div.adb
-- /division/ l+ ## 0
-- /in_handler/ l+ ## 0
