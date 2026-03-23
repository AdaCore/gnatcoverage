with Ctxt, Support; use Ctxt, Support;

procedure Test_Ctxt_Noraise is
begin
   Ctxt.Run (Iraise => False, Xraise => False);
end;

--# ctxt.adb
--  /pre/ l+ ## 0
--  /ieval/ l+ ## 0
--  /iraise/ l- ## s-
--  /xeval/ l+ ## 0
--  /xraise/ l- ## s-
--  /post/ l+ ## 0
--  /handler/ l- ## s-
--  /other/ l- ## s-
