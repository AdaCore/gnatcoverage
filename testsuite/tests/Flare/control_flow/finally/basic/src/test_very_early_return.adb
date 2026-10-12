pragma Flare_0_1;
with Pkg;

procedure Test_Very_Early_Return is
begin
   -- Pkg.Proc_Finally (True);

   Pkg.Decl_Finally (True, True);
   Pkg.Begin_Finally (True, True);
end Test_Very_Early_Return;

--  The finally block is not entered if its corresponding above block is
--  never entered either.

--#  pkg.adb
--   /finally/ l- ## s-
--
--   /not/     l- ## s-
--   /ret-2/   l- ## s-
--   /if-2/    l- ## s-
