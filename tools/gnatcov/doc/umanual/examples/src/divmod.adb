------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Divmod
  (X, Y : Integer; Value : out Integer;
   Divides : out Boolean; Tell : Boolean) is
begin
   if X mod Y = 0 then
      Divides := True;
      if Tell then
         Put_Line (Integer'Image (Y) & " divides " & Integer'Image (X));
      end if;
   else
      Divides := False;
   end if;

   Value := X / Y;
end Divmod;
