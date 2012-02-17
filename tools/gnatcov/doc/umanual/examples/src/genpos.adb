------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

package body Genpos is
   procedure Count (X : Num_T) is
   begin
      if X > 0 then
         N_Positive := N_Positive + 1;
      end if;
   end Count;
end Genpos;
