------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

function Div_With_Check (X, Y : Integer) return Integer is
begin
   if Y = 0 then
      raise Program_Error;
   else
      return X / Y;
   end if;
end Div_With_Check;
