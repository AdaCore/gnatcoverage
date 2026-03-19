with Ctxt, Doraise, Support; use Ctxt, Doraise, Support;

procedure Test_Ctxt_Noraise is
begin
   Ctxt.Run (Rk => None, Rm => None);
end;

--# ctxt.adb doraise.adb

--  common to both sources

--  /run/  l+ ## 0
--  /call_straight/       l- ## s-
--  /call_reraise_one/    l- ## s-
--  /call_reraise_other/  l- ## s-
--  /pre/  l+ ## 0
--  /post/ l+ ## 0
--  /ce_handler/ l- ## s-
--  /pe_handler/ l- ## s-
--  /ue_handler/ l- ## s-
--  /other/ l- ## s-

--  specific to doraise.adb

--  /do_raise-pre/ l- ## s-
--  /do_raise-post/ l- ## s-
--  /ice-eval/ l- ## s-
--  /ice-raise/ l- ## s-
--  /xpe-eval/ l- ## s-
--  /xpe-raise/ l- ## s-
--  /xue-eval/ l- ## s-
--  /xue-raise/ l- ## s-
--  /rts-eval/ l- ## s-
--  /rts-raise/ l- ## s-

--  /rm_none/ l+ ## 0
--  /reraise_one_ue_handler/ l- ## s-
--  /reraise_one_ce_handler/ l- ## s-
--  /reraise_one_pe_handler/ l- ## s-
--  /reraise_other_handler/  l- ## s-
