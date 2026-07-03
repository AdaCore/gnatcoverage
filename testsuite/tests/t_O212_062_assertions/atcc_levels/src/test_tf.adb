with Pkg; use Pkg;

procedure Test_TF is
   Dummy : Boolean;
begin
   Dummy := F (-1, -2);
   Dummy := F (200, -2);
end Test_TF;

--# pkg.ads
--
-- /body/ l+ ## 0
-- /pre1/ l+ ## 0
-- /pre2/ l! ## ac!:"Y > 100"
