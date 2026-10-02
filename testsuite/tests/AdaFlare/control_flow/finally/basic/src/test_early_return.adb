pragma Flare_0_1;
with Pkg;

procedure Test_Early_Return is
begin
   Pkg.Proc_Finally (True);
   Pkg.Decl_Finally (False, True);
   Pkg.Begin_Finally (False, True);
end Test_Early_Return;

--#  pkg.adb
--   /finally/ l+ ## 0
--
--   /not/     l- ## s-
--   /ret-1/   l- ## s-
