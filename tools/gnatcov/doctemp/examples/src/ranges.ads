------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

package Ranges is

   function Between (X1, X2, X : Integer) return Boolean;
   --  Whether X is between X1 and X2, inclusive and however they are ordered

end Ranges;
