pragma Flare_0_1;
with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Proc_Finally (Early_Return : Boolean) is
   begin
      if Early_Return then             -- # if-2
         return;                       -- # ret-2
      end if;
      Put_Line ("not early");          -- # not
   finally
      Put_Line ("THE END");            -- # finally
   end Proc_Finally;

   procedure Decl_Finally (Very_Early_Return, Early_Return : Boolean) is
   begin
      if Very_Early_Return then
         return;                       -- # ret-1
      end if;
      declare
      begin
         if Early_Return then          -- # if-2
            return;                    -- # ret-2
         end if;
         Put_Line ("not early");       -- # not
      finally
         Put_Line ("THE END");         -- # finally
      end;
   end Decl_Finally;

   procedure Begin_Finally (Very_Early_Return, Early_Return : Boolean) is
   begin
      if Very_Early_Return then
         return;                       -- # ret-1
      end if;
      begin
         if Early_Return then          -- # if-2
            return;                    -- # ret-2
         end if;
         Put_Line ("not early");       -- # not
      finally
         Put_Line ("THE END");         -- # finally
      end;
   end Begin_Finally;
end Pkg;
