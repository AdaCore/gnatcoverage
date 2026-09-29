with Pkg; use Pkg;

procedure Test_TX is
begin
   if F (-10) /= -5 then
      raise Program_Error;
   end if;
end Test_TX;

--# pkg.ads
--
-- /guard-1/  l!                  ## dF-
-- /conseq-1/ d=>l., a=>l+, c=>l! ## da=>0, c=>ac!:"I >= 5"
-- /guard-2/  l.                  ## 0
-- /conseq-2/ d=>l., ac=>l-       ## d=>0, ac=>a-
