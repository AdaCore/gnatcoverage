------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                     Copyright (C) 2012-2021, AdaCore                     --
------------------------------------------------------------------------------

procedure Assert (T : Boolean) is
begin
   if not T then
      raise Program_Error;
   end if;
end Assert;
