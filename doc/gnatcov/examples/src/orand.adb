------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2021, AdaCore                     --
------------------------------------------------------------------------------

function Orand (A, B, C : Boolean) return Boolean is
begin
   return (A or else B) and then C;
end Orand;
