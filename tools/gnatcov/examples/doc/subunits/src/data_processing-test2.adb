------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2025, AdaCore                     --
------------------------------------------------------------------------------

-- data_processing-test2.adb; test variation #2

separate (Data_Processing)
procedure Test is
begin
   Process (X => -8);
   pragma Assert (Internal_Data < 0);
end;
