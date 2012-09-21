--  Test driver for exemptions. It only "withes" the functional code, so the
--  only constructs that are expected to be reported as covered are those that
--  are executed/elaborated when the spec and body of the package Exemptions
--  are elaborated

with Exemptions;
with Support;    use Support;

procedure Test_Exemptions_No_Call is
begin
   Assert (Exemptions.X = 3);
   Assert (Exemptions.Y = 1);
   Assert (Exemptions.Z = 2);
end Test_Exemptions_No_Call;

--# exemptions.ads
-- /decl/              l+ ## 0
-- /xdecl/             l# ## x0
-- /negI/              l+ ## 0

--# exemptions.adb
-- /xswap/             l* ## x+
-- /swap_stmt/         l- ## s-

-- /factorial/         l- ## s-
-- /1_factorial/       l- ## s-
-- /xfactorial/        l* ## x+
-- /elsif_factorial/   l- ## s-
-- /rec_factorial/     l- ## s-

-- /another_swap/      l+ ## 0

-- /another_factorial/ l- ## s-
-- /in_loop_a_f/       l- ## s-
-- /in_if_a_f/         l- ## s-
-- /handler_a_f/       l- ## s-

-- /xelab_1/           l# ## x0
-- /elab/              l+ ## 0
-- /xelab_2/           l# ## x0

