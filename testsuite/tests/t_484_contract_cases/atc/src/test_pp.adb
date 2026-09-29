with Pkg; use Pkg;

procedure Test_PP is
   Self : constant T := (2, 3);
begin
   if Cartesian_Product (Self) /= 6 then
      raise Program_Error;
   end if;
end Test_PP;

--# pkg.ads
--
-- /guard-pp/      l!           ## dF-
-- /conseq-pp/     d=>l., a=>l+ ## 0
-- /guard-pn/      l!           ## dT-
-- /conseq-pn/     d=>l., a=>l- ## d=>0, a=>a-
-- /guard-np/      l!           ## dT-
-- /conseq-np/     d=>l., a=>l- ## d=>0, a=>a-
-- /guard-others/  l.           ## 0
-- /conseq-others/ d=>l., a=>l- ## d=>0, a=>a-
