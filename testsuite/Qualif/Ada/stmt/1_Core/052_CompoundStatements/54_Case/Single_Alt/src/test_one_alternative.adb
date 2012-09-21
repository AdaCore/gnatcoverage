--  Test driver for CASE statements. It executes all the CASE statements from
--  the functional code but does it only once for each statement.

with CASE_Statements;         use CASE_Statements;
with CASE_Statements_Support; use CASE_Statements_Support;
with More_CASE_Statements;    use More_CASE_Statements;
with Support;                 use Support;
procedure Test_One_Alternative is
   procedure My_Adjust_Int is new Adjust_Int_P (Integer, 10, 20);
   function My_Adjust_Int is new Adjust_Int_F (100, 200);

   Res  : Integer;
begin
   Res := 1;

   My_Adjust_Int (Res, 2, 1);
   Assert (Res = 10);

   Set_Prime_Number (Res, 1);
   Assert (Res = 2);

   Res := My_Adjust_Int (Res);
   Assert (Res = 100);

   --  Results of package body statements (elaboration sequence)
   Assert (Global_Int = 1);
   Assert (Global_Color = Red);
end Test_One_Alternative;

--# case_statements.adb
-- /colorcase/       l+ ## 0
-- /white/           l- ## s-
-- /red/             l- ## s-
-- /yellow/          l- ## s-
-- /green/           l+ ## 0
-- /blue/            l- ## s-
-- /brown/           l- ## s-
-- /black/           l- ## s-
-- /valcase/         l+ ## 0
-- /1case/           l+ ## 0
-- /2case/           l- ## s-
-- /4case/           l- ## s-
-- /7case/           l- ## s-
-- /others/          l- ## s-

--# more_case_statements.adb
-- /caseprime/       l+ ## 0
-- /1prime/          l+ ## 0
-- /2prime/          l- ## s-
-- /3prime/          l- ## s-
-- /4prime/          l- ## s-
-- /5prime/          l- ## s-
-- /6prime/          l- ## s-
-- /7prime/          l- ## s-
-- /8prime/          l- ## s-
-- /9prime/          l- ## s-
-- /10prime/         l- ## s-
-- /othersprim/      l- ## s-
-- /adjust/          l+ ## 0
-- /1adjust/         l+ ## 0
-- /2adjust/         l- ## s-
-- /100adjust/       l- ## s-
-- /othersadjust/    l- ## s-
-- /elab/            l+ ## 0
-- /1elab/           l+ ## 0
-- /2elab/           l- ## s-
-- /otherselabint/   l- ## s-
-- /whiteelab/       l+ ## 0
-- /blueelab/        l- ## s-
-- /otherselabcolor/ l- ## s-
