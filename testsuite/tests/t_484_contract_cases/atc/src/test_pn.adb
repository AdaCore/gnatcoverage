with Pkg; use Pkg;

procedure Test_PN is
   Self : constant T := (2, -3);
begin
   if Cartesian_Product (Self) /= -6 then
      raise Program_Error;
   end if;
end Test_PN;

--# pkg.ads
--
-- /guard-pp/      l!           ## dT-
-- /conseq-pp/     d=>l., a=>l- ## d=>0, a=>a-
-- /guard-pn/      l!           ## dF-
-- /conseq-pn/     d=>l., a=>l+ ## d=>0, a=>0
-- /guard-np/      l!           ## dT-
-- /conseq-np/     d=>l., a=>l- ## d=>0, a=>a-
-- /guard-others/  l.           ## 0
-- /conseq-others/ d=>l., a=>l- ## d=>0, a=>a-
