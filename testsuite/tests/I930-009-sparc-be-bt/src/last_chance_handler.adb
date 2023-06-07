with System.Machine_Code;

package body Last_Chance_Handler is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);
   begin
      loop
         System.Machine_Code.Asm ("mov 0, %%g1; ta 0", Volatile => True);
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
