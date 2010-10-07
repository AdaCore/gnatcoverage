--  Test driver for exemptions. It calls subprograms from the functional
--  code, and it executes a part of the code in a part of the exempted
--  sections.
--
--  This driver executes not all but only a part of exempted code.

with Exemptions;
with Multiple_Exemptions;
with Support;    use Support;

procedure Test_Exemptions_Exempted_Code_Call is
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

   I := 3; J := -1; K := 1;
   Multiple_Exemptions (I, J, K);
   --  Code in exemption section is executed
   Assert (I = 3 and then J = 2 and then K = 1);
end Test_Exemptions_Exempted_Code_Call;

--# exemptions.ads
-- /decl/              l+ 0
-- /ex_decl/           l+ 0
-- /negI/              l+ 0

--# exemptions.adb
-- /swap_decl/         l+ 0
-- /swap_stmt/         l+ 0

-- /factorial/         l+ 0
-- /1_factorial/       l- s-
-- /ex_factorial/      l+ 0
-- /elsif_factorial/   l+ 0
-- /rec_factorial/     l- s-

-- /another_swap/      l+ 0

-- /another_factorial/ l- s-
-- /in_loop_a_f/       l- s-
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
-- /2_exem/            l+ 0
-- /3_if/              l- s-
-- /3_exem/            l- s-
-- /handler/           l- s-
