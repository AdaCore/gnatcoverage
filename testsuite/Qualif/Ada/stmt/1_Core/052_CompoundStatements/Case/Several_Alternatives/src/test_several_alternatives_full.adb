--  Test driver for CASE statements. It executes all the case statements in the
--  elaboration code except the statements in the body of package
--  More_CASE_Statements many times so all the case alternatives of these
--  statements are finally executed.

with CASE_Statements;         use CASE_Statements;
with CASE_Statements_Support; use CASE_Statements_Support;
with More_CASE_Statements;    use More_CASE_Statements;
with Support;                 use Support;
procedure Test_Several_Alternatives_Full is
   procedure My_Adjust_Int is new Adjust_Int_P (Integer, 10, 20);
   function My_Adjust_Int is new Adjust_Int_F (100, 200);

   Res  : Integer;

begin
   --  CASE_Statements.Adjust_Color
   Assert (Adjust_Color (White)  = Red);
   Assert (Adjust_Color (Red)    = Yellow);
   Assert (Adjust_Color (Yellow) = Green);
   Assert (Adjust_Color (Green)  = Blue);
   Assert (Adjust_Color (Blue)   = Brown);
   Assert (Adjust_Color (Brown)  = Black);
   Assert (Adjust_Color (Black)  = White);

   --  CASE_Statements.Adjust_Int_P
   Res := 1;
   My_Adjust_Int (Res, 2, 1);
   Assert (Res = 10);

   Res := 2;
   My_Adjust_Int (Res, 2, 2);
   Assert (Res = 20);

   Res := 4;
   My_Adjust_Int (Res, 1, 2);
   Assert (Res = 1);

   Res := 7;
   My_Adjust_Int (Res, 1, 13);
   Assert (Res = 13);

   Res := 100;
   My_Adjust_Int (Res, 0, 0);
   Assert (Res = 0);

   --  More_CASE_Statements.Set_Prime_Number
   Set_Prime_Number (Res, 1);
   Assert (Res = 2);
   Set_Prime_Number (Res, 2);
   Assert (Res = 3);
   Set_Prime_Number (Res, 3);
   Assert (Res = 5);
   Set_Prime_Number (Res, 4);
   Assert (Res = 7);
   Set_Prime_Number (Res, 5);
   Assert (Res = 11);
   Set_Prime_Number (Res, 6);
   Assert (Res = 13);
   Set_Prime_Number (Res, 7);
   Assert (Res = 17);
   Set_Prime_Number (Res, 8);
   Assert (Res = 19);
   Set_Prime_Number (Res, 9);
   Assert (Res = 23);
   Set_Prime_Number (Res, 10);
   Assert (Res = 29);
   Set_Prime_Number (Res, 11);
   Assert (Res = 0);

   --  More_CASE_Statements.Adjust_Int_F
   Res := 10;
   Res := My_Adjust_Int (Res);
   Assert (Res = 100);

   Res := 20;
   Res := My_Adjust_Int (Res);
   Assert (Res = 200);

   Res := 100;
   Res := My_Adjust_Int (Res);
   Assert (Res = 200);

   Res := 500;
   Res := My_Adjust_Int (Res);
   Assert (Res = 700);

   --  Results of package body statements (elaboration sequence)
   Assert (Global_Int = 1);
   Assert (Global_Color = Red);
end Test_Several_Alternatives_Full;

--# case_statements.adb
-- /colorcase/       l+ 0
-- /white/           l+ 0
-- /red/             l+ 0
-- /yellow/          l+ 0
-- /green/           l+ 0
-- /blue/            l+ 0
-- /brown/           l+ 0
-- /black/           l+ 0
-- /valcase/         l+ 0
-- /1case/           l+ 0
-- /2case/           l+ 0
-- /4case/           l+ 0
-- /7case/           l+ 0
-- /others/          l+ 0

--# more_case_statements.adb
-- /caseprime/       l+ 0
-- /1prime/          l+ 0
-- /2prime/          l+ 0
-- /3prime/          l+ 0
-- /4prime/          l+ 0
-- /5prime/          l+ 0
-- /6prime/          l+ 0
-- /7prime/          l+ 0
-- /8prime/          l+ 0
-- /9prime/          l+ 0
-- /10prime/         l+ 0
-- /othersprim/      l+ 0
-- /adjust/          l+ 0
-- /1adjust/         l+ 0
-- /2adjust/         l+ 0
-- /100adjust/       l+ 0
-- /othersadjust/    l+ 0
-- /elab/            l+ 0
-- /1elab/           l+ 0
-- /2elab/           l- s-
-- /otherselabint/   l- s-
-- /whiteelab/       l+ 0
-- /blueelab/        l- s-
-- /otherselabcolor/ l- s-
