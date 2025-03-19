------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2025, AdaCore                     --
------------------------------------------------------------------------------

package body Sensors is

   function Value (Index : Sensor_Index_Range) return Integer is
   begin
      --  Dummy implementation, for demonstration purposes

      case Index is
         when 1 => return 1;
         when 2 => return 5;
         when 3 => return 3;
         when 4 => return 7;
      end case;
   end Value;

end Sensors;
