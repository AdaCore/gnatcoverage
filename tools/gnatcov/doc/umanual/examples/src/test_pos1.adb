------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support; use Support;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Pos1 is
   function Pos (X : Integer) return Boolean;
   pragma Inline (Pos);

   function Pos (X : Integer) return Boolean is
   begin
      if X > 0 then
         Put_Line ("X is positive");
         return True;
      else
         Put_Line ("X is not positive");
         return False;
      end if;
   end Pos;

begin
   Assert (Pos (1) = True);
end Test_Pos1;
