------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
--                                                                          --
------------------------------------------------------------------------------

with Services; use Services;

procedure Test_Services is

   procedure Assert (T : Boolean);

   procedure Assert (T : Boolean) is
   begin
      if not T then
         raise Program_Error;
      end if;
   end Assert;

begin
   Assert (Andthen (True, False) = False);
   Assert (Oor (True, True) = False);
end Test_Services;
