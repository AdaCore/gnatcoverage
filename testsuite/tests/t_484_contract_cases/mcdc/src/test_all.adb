with Pkg; use Pkg;

procedure Test_All is
begin
   if F (-10) /= -5 or else F (3) /= 1 or else F (10) /= 5 then
      raise Program_Error;
   end if;
end Test_All;

--# pkg.ads
--
-- /guard-1/  l+            ## 0
-- /conseq-1/ mu=>l., c=>l+ ## 0
-- /conseq-2/ mu=>l., c=>l+ ## 0
