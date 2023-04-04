with Ctxt, Support; use Ctxt, Support;

procedure Test_Ctxt_Xraise is
begin
   Ctxt.Run (Iraise => False, Xraise => True);
end;

--# ctxt.adb
--  /pre/ l+ ## 0
--  /ieval/ l+ ## 0
--  /iraise/ l- ## s-
--  /xeval/ l+ ## 0
--  /xraise/ l+ ## 0
--  /post/ l- ## s-
--  /handler/ l+ ## 0
--  /other/ l- ## s-

