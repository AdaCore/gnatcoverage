with Pkg; use Pkg;

procedure Test_TX is
begin
   if F (-10) /= -5 then
      raise Program_Error;
   end if;
end Test_TX;

--# pkg.ads
--
-- /guard-1/  l!            ## dF-
-- /conseq-1/ mu=>l., c=>l! ## mu=>0, c=>ac!:"I >= 5"
-- /conseq-2/ mu=>l., c=>l- ## mu=>0, c=>a-
