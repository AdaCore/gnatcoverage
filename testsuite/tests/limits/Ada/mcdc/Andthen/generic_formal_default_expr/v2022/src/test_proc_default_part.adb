with Proc;

procedure Test_Proc_Default_Part is
begin
   if Proc (False, False, False) then
      raise Program_Error;
   elsif not Proc (True, True, False) then
      raise Program_Error;
   end if;
end Test_Proc_Default_Part;

--# proc.adb
--
-- /decision/      l! ## c!:"BB"
-- /stmt/          l+ ## 0
-- /if-override/   l! ## dT-
-- /inst-override/ l- ## s-
-- /ret-override/  l- ## s-
-- /inst-default/  l+ ## 0
-- /ret-default/   l+ ## 0
