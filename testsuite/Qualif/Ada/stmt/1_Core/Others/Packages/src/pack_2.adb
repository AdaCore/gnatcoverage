with Pack_1;
package body Pack_2 is

   procedure Proc_2 (I : in out Integer) is
      package Local_Pack is
         J : Integer := 1;            -- # proc_2
      end;
      package body Local_Pack is
      begin
         Pack_1.Proc_1 (J);           -- # proc_2
      end Local_Pack;
   begin
      I := I + Local_Pack.J;          -- # proc_2
   end Proc_2;

begin
   I := Pack_1.Fun (I);               -- # elab
end Pack_2;
