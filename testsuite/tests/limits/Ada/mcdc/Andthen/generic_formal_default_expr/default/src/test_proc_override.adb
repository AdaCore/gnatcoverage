with Proc;

procedure Test_Proc_Override is
begin
   if Proc (False, False, True) then
      raise Program_Error;
   end if;
end Test_Proc_Override;

--# proc.adb
--
-- /decision/      l- ## e-
-- /stmt/          l+ ## 0
-- /if-override/   l! ## dF-
-- /inst-override/ l+ ## 0
-- /ret-override/  l+ ## 0
-- /inst-default/  l- ## s-
-- /ret-default/   l- ## s-
