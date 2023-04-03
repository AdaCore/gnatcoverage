with Ctxt, Doraise, Support; use Ctxt, Doraise, Support;

procedure Test_Ctxt_ICE_RO is
begin
   Ctxt.Run (Rk => Implicit_CE, Rm => Reraise_Other);
end;

--# ctxt.adb doraise.adb

--  common to both sources

--  /run/  l+ ## 0
--  /call_straight/       l- ## s-
--  /call_reraise_one/    l- ## s-
--  /call_reraise_other/  l+ ## 0
--  /pre/  l+ ## 0
--  /post/ l- ## s-
--  /ce_handler/ l+ ## 0
--  /pe_handler/ l- ## s-
--  /ue_handler/ l- ## s-
--  /other/ l- ## s-

--  specific to doraise.adb

--  /do_raise-pre/ l+ ## 0
--  /do_raise-post/ l- ## s-
--  /ice-eval/ l+ ## 0
--  /ice-raise/ l+ ## 0
--  /xpe-eval/ l- ## s-
--  /xpe-raise/ l- ## s-
--  /xue-eval/ l- ## s-
--  /xue-raise/ l- ## s-
--  /rts-eval/ l- ## s-
--  /rts-raise/ l- ## s-

--  /rm_none/ l- ## s-
--  /reraise_one_ce_handler/ l- ## s-
--  /reraise_one_pe_handler/ l- ## s-
--  /reraise_one_ue_handler/ l- ## s-
--  /reraise_other_handler/  l+ ## 0
