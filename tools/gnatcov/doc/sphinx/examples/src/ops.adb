------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body Ops is

   function Divides (X, Y : Integer) return Boolean is
   begin
      if Y mod X = 0 then
         Put_Line (Integer'Image (X) & " divides " & Integer'Image (Y));
         return True;
      else
         return False;
      end if;
   end Divides;

end Ops;
