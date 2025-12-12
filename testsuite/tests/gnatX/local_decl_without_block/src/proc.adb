pragma Extensions_Allowed (On);

with Ada.Text_IO; use Ada.Text_IO;

procedure Proc (I : Integer) is
begin
   if I = 0 then              -- # before_0
      Put_Line ("I is null"); -- # if_0
      return;                 -- # if_0
   end if;

   Ten_Times : constant Integer := 10 * I;   -- # before_100
   Put_Line ("10 * I = " & Ten_Times'Image); -- # before_100
   if I < 100 then                           -- # before_100
      Put_Line ("I has less than 3 digits"); -- # if_lt_100
      return;                                -- # if_lt_100
   end if;

   Last_Digit : constant Integer := I mod 10;     -- # final
   Put_Line ("Last digit = " & Last_Digit'Image); -- # final
end Proc;
