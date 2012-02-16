------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

generic
   type Num_T is range <>;
package Genpos is
   procedure Count (X : Num_T);
   --  Increment N_Positive is X > 0

   N_Positive : Natural := 0;
   --  Number of positive values passed to Count
end Genpos;
