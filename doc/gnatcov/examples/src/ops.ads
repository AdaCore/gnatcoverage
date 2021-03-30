------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2021, AdaCore                     --
------------------------------------------------------------------------------

package Ops is
   function Divides (X, Y : Integer) return Boolean;
   --  Whether Y mod X is 0, outputting a message telling so
   --  when True to standard output.
end Ops;
