------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Genpos;
package POSI is

   type T1 is new Integer;
   package Pos_T1 is new Genpos (Num_T => T1);

   type T2 is new Integer;
   package Pos_T2 is new Genpos (Num_T => T2);
end POSI;
