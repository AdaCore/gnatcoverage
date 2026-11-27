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
-- /conseq-1/ d=>l., ac=>l+ ## 0
-- /guard-2/  l.            ## 0
-- /conseq-2/ d=>l., ac=>l- ## d=>0, ac=>a-
