------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2025, AdaCore                     --
------------------------------------------------------------------------------

package Sensors is

   type Sensor_Index_Range is range 1 .. 4;

   function Value (Index : Sensor_Index_Range) return Integer;

end Sensors;
