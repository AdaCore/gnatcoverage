with Pkg; use Pkg;

procedure Test_0 is
   Self : constant T := (0, 0);
begin
   null;
end Test_0;

--# pkg.ads
--
-- /guard-pp/      l-           ## d-
-- /conseq-pp/     d=>l., a=>l- ## d=>0, a=>a-
-- /guard-pn/      l-           ## d-
-- /conseq-pn/     d=>l., a=>l- ## d=>0, a=>a-
-- /guard-np/      l-           ## d-
-- /conseq-np/     d=>l., a=>l- ## d=>0, a=>a-
-- /guard-others/  l.           ## 0
-- /conseq-others/ d=>l., a=>l- ## d=>0, a=>a-
