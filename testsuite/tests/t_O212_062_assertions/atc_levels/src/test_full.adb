with Pkg; use Pkg;

procedure Test_Full is
   Dummy : Boolean;
begin
   Dummy := F (1, 2);
end Test_Full;

--# pkg.ads
--
-- /body/ l+ ## 0
-- /pre1/ l+ ## 0
-- /pre2/ l+ ## 0
