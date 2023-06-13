with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Main is

   Val : Boolean := False;
   pragma Volatile (Val);

   task T;

   task body T is
   begin
      delay until Clock + Seconds (5);
      Put_Line ("In Task");
      Val := True;
   end T;

begin
   Put_Line ("In main");
end Main;