------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2021, AdaCore                     --
------------------------------------------------------------------------------

package Ranges is

   function Between (X1, X2, V : Integer) return Boolean;
   --  Whether V is between X1 and X2, inclusive and however they are ordered

end Ranges;
