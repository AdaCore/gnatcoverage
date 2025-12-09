with Pkg; use Pkg;

procedure Test_Pkg is
   Dummy : Boolean;
begin
   Dummy := F1 (10);

   --  The following two calls ensure that both outcomes in F2's decisions are
   --  covered. However MC/DC coverage is not supposed to be achieved.

   Dummy := F2 (0, 0, 0);
   Dummy := F2 (1, 1, 1);
end Test_Pkg;

--# pkg.ads
--
-- /f1-pre/  l+ ## 0
-- /f1-body/ l+ ## 0
-- /f2-body/ l! ## c!:"Y" c!:"Z"
