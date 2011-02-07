--  Test driver for CASE statements. It executes each of the case statements in
--  the elaboration code except the statements in the body of package
--  More_CASE_Statements twice, if the case statement contains an alternative
--  with OTHERS choice, this alternative is not executed.

with CASE_Statements;         use CASE_Statements;
with CASE_Statements_Support; use CASE_Statements_Support;
with More_CASE_Statements;    use More_CASE_Statements;
with Support;                 use Support;
procedure Test_Several_Alternatives_No_Others is
   procedure My_Adjust_Int is new Adjust_Int_P (Integer, 10, 20);
   function My_Adjust_Int is new Adjust_Int_F (100, 200);

   Res  : Integer;

begin
   --  CASE_Statements.Adjust_Color
   Assert (Adjust_Color (Blue)  = Brown);
   Assert (Adjust_Color (Brown) = Black);

   --  CASE_Statements.Adjust_Int_P
   Res := 5;
   My_Adjust_Int (Res, 2, 2);
   Assert (Res = 2);

   Res := 10;
   My_Adjust_Int (Res, 0, 0);
   Assert (Res = 0);

   --  More_CASE_Statements.Set_Prime_Number
   Set_Prime_Number (Res, 3);
   Assert (Res = 5);
   Set_Prime_Number (Res, 4);
   Assert (Res = 7);

   --  More_CASE_Statements.Adjust_Int_F

   Res := 1;
   Res := My_Adjust_Int (Res);
   Assert (Res = 100);

   Res := 100;
   Res := My_Adjust_Int (Res);
   Assert (Res = 200);

   --  Results of package body statements (elaboration sequence)
   Assert (Global_Int = 1);
   Assert (Global_Color = Red);
end Test_Several_Alternatives_No_Others;

--# case_statements.adb
-- /colorcase/       l+ 0
-- /white/           l- s-
-- /red/             l- s-
-- /yellow/          l- s-
-- /green/           l+ 0
-- /blue/            l+ 0
-- /brown/           l+ 0
-- /black/           l- s-
-- /valcase/         l+ 0
-- /1case/           l- s-
-- /2case/           l- s-
-- /4case/           l+ 0
-- /7case/           l+ 0
-- /others/          l- s-

--# more_case_statements.adb
-- /caseprime/       l+ 0
-- /1prime/          l- s-
-- /2prime/          l- s-
-- /3prime/          l+ 0
-- /4prime/          l+ 0
-- /5prime/          l- s-
-- /6prime/          l- s-
-- /7prime/          l- s-
-- /8prime/          l- s-
-- /9prime/          l- s-
-- /10prime/         l- s-
-- /othersprim/      l- s-
-- /adjust/          l+ 0
-- /1adjust/         l+ 0
-- /2adjust/         l- s-
-- /100adjust/       l+ 0
-- /othersadjust/    l- s-
-- /elab/            l+ 0
-- /1elab/           l+ 0
-- /2elab/           l- s-
-- /otherselabint/   l- s-
-- /whiteelab/       l+ 0
-- /blueelab/        l- s-
-- /otherselabcolor/ l- s-
