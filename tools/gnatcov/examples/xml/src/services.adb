------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
--                                                                          --
------------------------------------------------------------------------------

package body Services is
   function Andthen (A, B : Boolean) return Boolean is
   begin
      return A and then B;
   end Andthen;

   function Orelse (A, B : Boolean) return Boolean is
   begin
      return A or else B;
   end Orelse;

   function Oor (A, B : Boolean) return Boolean is
   begin
      if Orelse (A, B) and then not Andthen (A, B) then
         return True;
      else
         return False;
      end if;
   end Oor;
end Services;
