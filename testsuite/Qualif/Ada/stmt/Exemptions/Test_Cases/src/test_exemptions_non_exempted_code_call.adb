--  Test driver for exemptions. It calls subprograms from the functional
--  code, but these calls do not execute/elaborate exempted code. So the only
--  exempted construct that are expected to be reported as covered are those
--  that are executed/elaborated when the spec and body of the package
--  Exemptions are elaborated
--
--  This driver executes not all but only a part of non-exempted code.

with Exemptions;
with Multiple_Exemptions;
with Support;    use Support;

procedure Test_Exemptions_Non_Exempted_Code_Call is
   I, J, K : Integer := 1;
begin
   Assert (Exemptions.X = 3);
   Assert (Exemptions.Y = 1);
   Assert (Exemptions.Z = 2);

   Assert (Exemptions.Factorial (3) = 6);
   --  No exempted code is executed

   Assert (Exemptions.Another_Factorial (3) = 6);
   --  No exempted code in this function

   Multiple_Exemptions (i, J, K);
   Assert (I = 1 and then J = 2 and then K = 1);
end Test_Exemptions_Non_Exempted_Code_Call;

--# exemptions.ads
-- /decl/              l+ 0
-- /ex_decl/           l+ 0
-- /negI/              l+ 0

--# exemptions.adb
-- /swap_decl/         l- s-
-- /swap_stmt/         l- s-

-- /factorial/         l+ 0
-- /1_factorial/       l+ 0
-- /ex_factorial/      l- s-
-- /elsif_factorial/   l+ 0
-- /rec_factorial/     l+ 0

-- /another_swap/      l+ 0

-- /another_factorial/ l+ 0
-- /in_loop_a_f/       l+ 0
-- /in_if_a_f/         l- s-
-- /handler_a_f/       l- s-

-- /1_elab/            l+ 0
-- /elab/              l+ 0
-- /2_elab/            l+ 0

--# multiple_exemptions.adb
-- /dcl/               l+ 0
-- /1_if/              l+ 0
-- /1_exem/            l- s-
-- /stmt/              l+ 0
-- /2_if/              l+ 0
-- /2_exem/            l- s-
-- /3_if/              l- s-
-- /3_exem/            l- s-
-- /handler/           l- s-
