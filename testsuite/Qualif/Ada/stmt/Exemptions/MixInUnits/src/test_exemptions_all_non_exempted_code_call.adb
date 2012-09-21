--  Test driver for exemptions. It calls subprograms from the functional
--  code, but these calls do not execute/elaborate exempted code. So the only
--  exempted construct that are expected to be reported as covered are those
--  that are executed/elaborated when the spec and body of the package
--  Exemptions are elaborated
--
--  This driver executes all the non-exempted code.

with Exemptions;
with Support;    use Support;

procedure Test_Exemptions_All_Non_Exempted_Code_Call is
   I, J, K : Integer := 1;
begin
   Assert (Exemptions.X = 3);
   Assert (Exemptions.Y = 1);
   Assert (Exemptions.Z = 2);

   Assert (Exemptions.Factorial (3) = 6);
   --  No exempted code is executed

   Assert (Exemptions.Another_Factorial (3) = 6);
   Assert (Exemptions.Another_Factorial (Positive'Last) = Positive'Last);
   --  No exempted code in this function

end Test_Exemptions_All_Non_Exempted_Code_Call;

--# exemptions.ads
-- /decl/              l+ ## 0
-- /xdecl/             l# ## x0
-- /negI/              l+ ## 0

--# exemptions.adb
-- /xswap/             l* ## x+
-- /swap_stmt/         l- ## s-

-- /factorial/         l+ ## 0
-- /1_factorial/       l+ ## 0
-- /xfactorial/        l* ## x+
-- /elsif_factorial/   l+ ## 0
-- /rec_factorial/     l+ ## 0

-- /another_swap/      l+ ## 0

-- /another_factorial/ l+ ## 0
-- /in_loop_a_f/       l+ ## 0
-- /in_if_a_f/         l+ ## 0
-- /handler_a_f/       l+ ## 0

-- /xelab_1/           l# ## x0
-- /elab/              l+ ## 0
-- /xelab_2/           l# ## x0
