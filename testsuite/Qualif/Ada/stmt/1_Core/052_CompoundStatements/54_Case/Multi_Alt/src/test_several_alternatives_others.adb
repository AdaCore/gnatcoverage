--  Test driver for CASE statements. It executes each of the case statements in
--  the elaboration code except the statements in the body of package
--  More_CASE_Statements twice, if the case statement contains an alternative
--  with OTHERS choice, this alternative is executed.

with CASE_Statements;         use CASE_Statements;
with CASE_Statements_Support; use CASE_Statements_Support;
with More_CASE_Statements;    use More_CASE_Statements;
with Support;                 use Support;
procedure Test_Several_Alternatives_Others is
   procedure My_Adjust_Int is new Adjust_Int_P (Integer, 10, 20);
   function My_Adjust_Int is new Adjust_Int_F (100, 200);

   Res  : Integer;

begin
   --  CASE_Statements.Adjust_Color
   Assert (Adjust_Color (Black)  = White);

   --  CASE_Statements.Adjust_Int_P
   Res := 2;
   My_Adjust_Int (Res, 2, 2);
   Assert (Res = 20);

   Res := 100;
   My_Adjust_Int (Res, 0, 0);
   Assert (Res = 0);

   --  More_CASE_Statements.Set_Prime_Number
   Set_Prime_Number (Res, 2);
   Assert (Res = 3);
   Set_Prime_Number (Res, 11);
   Assert (Res = 0);

   --  More_CASE_Statements.Adjust_Int_F

   Res := 20;
   Res := My_Adjust_Int (Res);
   Assert (Res = 200);

   Res := 500;
   Res := My_Adjust_Int (Res);
   Assert (Res = 700);

   --  Results of package body statements (elaboration sequence)
   Assert (Global_Int = 1);
   Assert (Global_Color = Red);
end Test_Several_Alternatives_Others;

--# case_statements.adb
-- /colorcase/       l+ ## 0
-- /white/           l- ## s-
-- /red/             l- ## s-
-- /yellow/          l- ## s-
-- /green/           l+ ## 0
-- /blue/            l- ## s-
-- /brown/           l- ## s-
-- /black/           l+ ## 0
-- /valcase/         l+ ## 0
-- /1case/           l- ## s-
-- /2case/           l+ ## 0
-- /4case/           l- ## s-
-- /7case/           l- ## s-
-- /others/          l+ ## 0

--# more_case_statements.adb
-- /caseprime/       l+ ## 0
-- /1prime/          l- ## s-
-- /2prime/          l+ ## 0
-- /3prime/          l- ## s-
-- /4prime/          l- ## s-
-- /5prime/          l- ## s-
-- /6prime/          l- ## s-
-- /7prime/          l- ## s-
-- /8prime/          l- ## s-
-- /9prime/          l- ## s-
-- /10prime/         l- ## s-
-- /othersprim/      l+ ## 0
-- /adjust/          l+ ## 0
-- /1adjust/         l- ## s-
-- /2adjust/         l+ ## 0
-- /100adjust/       l- ## s-
-- /othersadjust/    l+ ## 0
-- /elab/            l+ ## 0
-- /1elab/           l+ ## 0
-- /2elab/           l- ## s-
-- /otherselabint/   l- ## s-
-- /whiteelab/       l+ ## 0
-- /blueelab/        l- ## s-
-- /otherselabcolor/ l- ## s-
