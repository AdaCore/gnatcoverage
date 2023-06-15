with State; use State;

--  # out is used to mark statements that we expect to be taken out always.
--  # opt_out is for statements taken out only when optimizing.

package body Misc is
   
   -- A local library level subprogram just unused.
   
   procedure Dump_Old_Log;

   procedure Dump_Old_Log is
   begin
      Dtick; -- # opt_out
   end;
   
   -- A local subprogram which only gets called in a section guarded by a
   -- constant False test:
   
   procedure Dump_Debug_Log is
   begin
      Dtick; -- # opt_out
   end;
   
   procedure Check_State is
      
      --  A nested subprogram just unused
      
      procedure Crash_Me is
      begin
         Dtick;  -- # out
      end;
      
      --  A nested subprogram called in a guarded false
      --  sequence of statements

      procedure Maybe_Crash_Me is
      begin
         Dtick;  -- # out
      end;
      
   begin
      Tick; -- # stmt
      if GF then         -- # out
         Dump_Debug_Log; -- # out
         Maybe_Crash_Me; -- # out
      end if;
   end;
         
end;
