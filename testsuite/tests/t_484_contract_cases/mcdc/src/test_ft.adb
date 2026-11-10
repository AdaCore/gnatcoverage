with Pkg; use Pkg;

procedure Test_FT is
begin
   if F (10) /= 5 then
      raise Program_Error;
   end if;
end Test_FT;

--# pkg.ads
--
-- /guard-1/  l!            ## dF-
-- /conseq-1/ mu=>l., c=>l+ ## mu=>0, c=>0
-- /conseq-2/ mu=>l., c=>l- ## mu=>0, c=>a-
