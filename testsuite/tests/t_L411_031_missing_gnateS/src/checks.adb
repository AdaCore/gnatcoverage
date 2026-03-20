package body Checks is

   --  To control compilation options, we produce our own local project,
   --  without any libsupport consideration. We must make sure that we don't
   --  need a libsupport at all:

   pragma Suppress (All_Checks);

   procedure Assert (Cond : Boolean) is
   begin
      if not Cond then
         Assert_Failures := Assert_Failures + 1;
      end if;
   end;
end;
