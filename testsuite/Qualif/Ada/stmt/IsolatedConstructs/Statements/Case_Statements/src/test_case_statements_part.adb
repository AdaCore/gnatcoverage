--  Test driver for CASE statements. It instantiates all the generics from
--  functional code, and calls all the routines from it, and all the
--  instantiations, but each routine/instantiation is called only once, so
--  only a part of the code corresponding to CASE statements is expected to
--  be reported as covered.

with CASE_Statements;         use CASE_Statements;
with CASE_Statements_Support; use CASE_Statements_Support;
with More_CASE_Statements;    use More_CASE_Statements;
with Support;                 use Support;
procedure Test_CASE_Statements_Part is
   procedure My_Adjust_Int is new Adjust_Int_P (Integer, 10, 20);
   function My_Adjust_Int is new Adjust_Int_F (100, 200);

   Res  : Integer;
begin
   Res := 1;

   --  CASE_Statements.Adjust_Color
   Assert (Adjust_Color (White) = Red);

   --  CASE_Statements.Adjust_Int_P
   My_Adjust_Int (Res, 2, 1);
   Assert (Res = 10);

   --  More_CASE_Statements.Set_Prime_Number
   Set_Prime_Number (Res, 1);
   Assert (Res = 2);

   --  More_CASE_Statements.Adjust_Int_F
   Res := My_Adjust_Int (Res);
   Assert (Res = 100);

   --  Results of package body statements (elaboration sequence)
   Assert (Global_Int = 1);
   Assert (Global_Color = Red);
end Test_CASE_Statements_Part;

--# case_statements.adb
-- /colorcase/ l+ 0
-- /white/     l+ 0
-- /red/       l- s-
-- /yellow/    l- s-
-- /green/     l+ 0
-- /blue/      l- s-
-- /brown/     l- s-
-- /black/     l- s-
-- /valcase/   l+ 0
-- /1case/     l+ 0
-- /2case/     l- s-
-- /4case/     l- s-
-- /7case/     l- s-
-- /others/    l- s-

--# more_case_statements.adb
-- /caseprime/       l+ 0
-- /1prime/          l+ 0
-- /2prime/          l- s-
-- /3prime/          l- s-
-- /4prime/          l- s-
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
-- /100adjust/       l- s-
-- /othersadjust/    l- s-
-- /elab/            l+ 0
-- /1elab/           l+ 0
-- /2elab/           l- s-
-- /otherselabint/   l- s-
-- /whiteelab/       l+ 0
-- /blueelab/        l- s-
-- /otherselabcolor/ l- s-
