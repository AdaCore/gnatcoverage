with Ctxt, Support; use Ctxt, Support;

procedure Test_Ctxt_Iraise is
begin
   Ctxt.Run (Iraise => True, Xraise => False);
end;

--# ctxt.adb
--  /pre/ l+ ## 0
--  /ieval/ l+ ## 0
--  /iraise/ l+ ## 0
--  /xeval/ l- ## s-
--  /xraise/ l- ## s-
--  /post/ l- ## s-
--  /handler/ l+ ## 0
--  /other/ l- ## s-

