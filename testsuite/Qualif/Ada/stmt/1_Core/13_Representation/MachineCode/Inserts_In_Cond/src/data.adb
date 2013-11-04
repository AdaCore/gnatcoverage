with System.Machine_Code; use System.Machine_Code;

package body Data is
   function Id (X, Pathno: Integer) return Integer is
      Copy : Integer := X;
      pragma Volatile (Copy);      
   begin
      if Pathno = 1 then
         Asm ("# stw %1, %0", Volatile => True,  -- # asm_1
              Outputs => Integer'Asm_Output ("=m", Copy), -- # extra_1
              Inputs => Integer'Asm_Input ("r", X));     -- # extra_1
      else
         Asm ("# stw %1, %0",  Volatile => True, -- # asm_n
              Outputs => Integer'Asm_Output ("=m", Copy),  -- # extra_n
              Inputs => Integer'Asm_Input ("r", X));      -- # extra_n
      end if;
      return Copy;
   end;
   
end;
