------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2025, AdaCore                     --
------------------------------------------------------------------------------

package body Data_Processing is

   Internal_Data : Integer := 0;

   procedure Process (X : Integer) is
   begin
      if X < 0 then
         Internal_Data := -1;
      else
         Internal_Data := 1;
      end if;
   end;

   procedure Test is separate; -- subunit declaration here
end;
