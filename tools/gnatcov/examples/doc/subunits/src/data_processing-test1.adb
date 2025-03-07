------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2025, AdaCore                     --
------------------------------------------------------------------------------

-- data_processing-test1.adb; test variation #1

separate (Data_Processing)
procedure Test is
begin
   Process (X => 12);
   pragma Assert (Internal_Data > 0);
end;
