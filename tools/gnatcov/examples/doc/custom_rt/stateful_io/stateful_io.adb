with GNAT.IO;

package body Stateful_IO is

   --  Since we are short-circuiting the Ada compiler's elaboration checks,
   --  preserve a manual guard to detect attempts to send data to the IO
   --  channel before elaboration has occurred.

   Initialized : Boolean := False;

   procedure Put (S : String) is
   begin
      if not Initialized then
         raise Program_Error
           with "attempt to call Stateful_IO.Put before elaboration";
      end if;

      --  Replace the following with the actual code to send data to the IO
      --  stream used by coverage data.

      GNAT.IO.Put (S);
   end Put;

begin
   --  Here, do whatever necessary to initialize the IO stream

   Initialized := True;
end Stateful_IO;
