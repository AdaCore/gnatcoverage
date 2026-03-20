with Proc;

procedure Test_Proc_Default is
begin
   if Proc (False, False, False) then
      raise Program_Error;
   end if;
end Test_Proc_Default;

--# proc.adb
--
-- /decision/      l! ## eT-
-- /stmt/          l+ ## 0
-- /if-override/   l! ## dT-
-- /inst-override/ l- ## s-
-- /ret-override/  l- ## s-
-- /inst-default/  l+ ## 0
-- /ret-default/   l+ ## 0
