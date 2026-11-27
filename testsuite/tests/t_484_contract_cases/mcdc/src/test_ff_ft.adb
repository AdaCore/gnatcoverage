with Pkg; use Pkg;

procedure Test_FF_FT is
begin
   if F (3) /= 1 or else F (10) /= 5 then
      raise Program_Error;
   end if;
end Test_FF_FT;

--# pkg.ads
--
-- /guard-1/  l!            ## c!:"I <= 0"
-- /conseq-1/ mu=>l., c=>l+ ## 0
-- /conseq-2/ mu=>l., c=>l+ ## 0
