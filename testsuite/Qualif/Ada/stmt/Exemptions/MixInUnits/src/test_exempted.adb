--  Test driver for exemptions. It calls subprograms from the functional
--  code, and it executes a part of the code in a part of the exempted
--  sections.
--
--  This driver executes not all but only a part of exempted code.

with Exemptions;
with Support;    use Support;

procedure Test_Exempted is
   I : Integer := 1;
   J : Integer := 2;
   K : Integer := 3;
begin
   Assert (Exemptions.X = 3);
   Assert (Exemptions.Y = 1);
   Assert (Exemptions.Z = 2);

   Exemptions.Swap (I, J);
   Assert (I = 2 and then J = 1);

   Assert (Exemptions.Factorial (Positive'Last) = Positive'Last);
   --  Code in exemption section is executed

end Test_Exempted;

--# exemptions.ads
-- /decl/              l+ ## 0
-- /xdecl/             l# ## x0
-- /negI/              l+ ## 0

--# exemptions.adb
-- /xswap/             l# ## x0
-- /swap_stmt/         l+ ## 0

-- /factorial/         l+ ## 0
-- /1_factorial/       l- ## s-
-- /xfactorial/        l# ## x0
-- /elsif_factorial/   l+ ## 0
-- /rec_factorial/     l- ## s-

-- /another_swap/      l+ ## 0

-- /another_factorial/ l- ## s-
-- /in_loop_a_f/       l- ## s-
-- /in_if_a_f/         l- ## s-
-- /handler_a_f/       l- ## s-

-- /xelab_1/           l# ## x0
-- /elab/              l+ ## 0
-- /xelab_2/           l# ## x0
