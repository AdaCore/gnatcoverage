with GNAT.IO; use GNAT.IO;

package body Logging is

   procedure Write (Prefix, Message : String) is
   begin
      Put (Prefix);
      Put (Message);
      New_Line;
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
