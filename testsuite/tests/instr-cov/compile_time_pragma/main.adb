with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   pragma Compile_Time_Error
     (Boolean'Size /= 1 or else Integer'Size < 2, "Dummy error check");
   pragma Compile_Time_Warning (Character'Size /= 8, "Dummy warning check");
   Put_Line ("Hello, world!");
end Main;
