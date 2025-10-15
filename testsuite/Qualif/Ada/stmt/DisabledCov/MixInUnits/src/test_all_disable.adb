--  Test driver for disabled coverage regions. It calls subprograms from the
--  functional code, and it executes all the code in all the disabled coverage
--  regions.

with Disabled;
with Support; use Support;

procedure Test_All_Disable is
   I : Integer := 1;
   J : Integer := 2;
   K : Integer := 3;
begin
   Assert (Disabled.X = 3);
   Assert (Disabled.Y = 1);
   Assert (Disabled.Z = 2);

   Disabled.Swap (I, J);
   Assert (I = 2 and then J = 1);

   Assert (Disabled.Factorial (Positive'Last) = Positive'Last);
   --  Code in disabled coverage region is executed

end Test_All_Disable;

--# disabled.ads
-- /disabled/          lD ## 0
-- /negI/              l+ ## 0

--# disabled.adb
-- /disabled/          lD ## 0
-- /swap_stmt/         l+ ## 0
-- /factorial/         l+ ## 0
-- /1_factorial/       l- ## s-
-- /elsif_factorial/   l+ ## 0
-- /rec_factorial/     l- ## s-

-- /another_swap/      l+ ## 0

-- /another_factorial/ l- ## s-
-- /in_loop_a_f/       l- ## s-
-- /in_if_a_f/         l- ## s-
-- /handler_a_f/       l- ## s-

-- /elab/              l+ ## 0
