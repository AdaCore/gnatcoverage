with Ctxt, Doraise, Support; use Ctxt, Doraise, Support;

procedure Test_Ctxt_RTS_ST is
begin
   Ctxt.Run (Rk => From_RTS, Rm => Straight);
end;

--# ctxt.adb doraise.adb

--  common to both sources

--  /run/   l+ ## 0
--  /call_straight/       l+ ## 0
--  /call_reraise_one/    l- ## s-
--  /call_reraise_other/  l- ## s-
--  /pre/ l+ ## 0
--  /post/ l- ## s-
--  /ce_handler/ l+ ## 0
--  /pe_handler/ l- ## s-
--  /ue_handler/ l- ## s-
--  /other/ l- ## s-

--  specific to doraise.adb

--  /do_raise-pre/ l+ ## 0
--  /do_raise-post/ l- ## s-
--  /ice-eval/ l+ ## 0
--  /ice-raise/ l- ## s-
--  /xpe-eval/ l+ ## 0
--  /xpe-raise/ l- ## s-
--  /xue-eval/ l+ ## 0
--  /xue-raise/ l- ## s-
--  /rts-eval/ l+ ## 0
--  /rts-raise/ l+ ## 0

--  /rm_none/ l- ## s-
--  /reraise_one_ue_handler/ l- ## s-
--  /reraise_one_ce_handler/ l- ## s-
--  /reraise_one_pe_handler/ l- ## s-
--  /reraise_other_handler/  l- ## s-
