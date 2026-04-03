with Ada.Text_IO; use Ada.Text_IO;

package body Logging is

   procedure Write (Prefix, Message : String) is
   begin
      Put (Prefix);
      Put_Line (Message);
   end Write;

   procedure Warn (Message : String) is
   begin
      Write ("warning: ", Message);
   end Warn;

   procedure Error (Message : String) is
   begin
      Write ("error: ", Message);
   end Error;
end Logging;
