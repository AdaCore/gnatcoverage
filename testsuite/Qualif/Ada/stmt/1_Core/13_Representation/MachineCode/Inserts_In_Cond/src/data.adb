with System.Machine_Code; use System.Machine_Code;

package body Data is
   
   function Id (X, Pathno: Integer) return Integer is
      Copy : Integer := X;
      pragma Volatile (Copy);      
   begin
      if Pathno = 1 then
         Asm ("nop", Volatile => True);  -- # asm_1
      else
         Asm ("nop", Volatile => True);  -- # asm_n
      end if;
      return Copy;
   end;
   
end;
