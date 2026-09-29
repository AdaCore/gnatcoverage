with Pkg; use Pkg;

procedure Test_FF is
begin
   if F (3) /= 1 then
      raise Program_Error;
   end if;
end Test_FF;

--# pkg.ads
--
-- /guard-1/  l!            ## dT-
-- /conseq-1/ mu=>l., c=>l- ## mu=>0, c=>a-
-- /conseq-2/ mu=>l., c=>l+ ## mu=>0, c=>0
