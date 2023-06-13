with Support; use Support;
with Div; use Div;

procedure Test_Divide_Ok is

   Result : Integer;

begin

   Divide (12, 2, Result);

   Assert (N_In_Handler = 0);

end Test_Divide_Ok;

--# div.adb
-- /division/ l+ ## 0
-- /in_handler/ l- ## s-
