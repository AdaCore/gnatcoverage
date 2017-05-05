------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2008-2017, AdaCore                      --
--                                                                          --
------------------------------------------------------------------------------

package body Support is
   procedure Assert (Cond : Boolean) is
   begin
      if not Cond then
         raise Program_Error;
      end if;
   end Assert;
end Support;
