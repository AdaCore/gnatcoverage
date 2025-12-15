with Proc;

procedure Test_Full is
begin
   Proc (False);
   Proc (True);
end Test_Full;

--# proc.adb
--
-- /subtype_decl/ l+ ## 0
-- /stmt/         l+ ## 0
