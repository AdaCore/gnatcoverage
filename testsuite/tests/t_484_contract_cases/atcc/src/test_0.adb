with Pkg; use Pkg;

procedure Test_0 is
begin
   null;
end Test_0;

--# pkg.ads
--
-- /guard-1/  l-            ## d-
-- /conseq-1/ d=>l., ac=>l- ## d=>0, ac=>a-
-- /guard-2/  l.            ## 0
-- /conseq-2/ d=>l., ac=>l- ## d=>0, ac=>a-
