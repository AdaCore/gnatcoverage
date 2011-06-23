with Pack_1;
procedure Library_Level_Proc (I : in out Integer) is
   package Inner_Pack is
      J : Integer := Pack_1.Fun (1);  -- # proc
   end Inner_Pack;

   package body Inner_Pack is
   begin
      Pack_1.Proc_1 (J);              -- # proc
   end Inner_Pack;
begin
   I := I + Inner_Pack.J;             -- # proc
end Library_Level_Proc;
