------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Div_With_Check, Ada.Text_IO; use Support, Ada.Text_IO;

procedure Test_Div0  is
   Result : Integer
     := Div_With_Check (4, 0);
begin
   Put_Line ("R = " & Integer'Image (Result));
end Test_Div0;
