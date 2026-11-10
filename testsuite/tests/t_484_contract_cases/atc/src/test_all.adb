with Pkg; use Pkg;

procedure Test_All is
   Self : T := (2, 3);
begin
   if Cartesian_Product (Self) /= 6 then
      raise Program_Error;
   end if;

   Self.Y := -3;
   if Cartesian_Product (Self) /= -6 then
      raise Program_Error;
   end if;

   Self.X := -2;
   if Cartesian_Product (Self) /= 6 then
      raise Program_Error;
   end if;

   Self.Y := 3;
   if Cartesian_Product (Self) /= -6 then
      raise Program_Error;
   end if;
end Test_All;

--# pkg.ads
--
-- /guard-pp/      l+           ## 0
-- /conseq-pp/     d=>l., a=>l+ ## 0
-- /guard-pn/      l+           ## 0
-- /conseq-pn/     d=>l., a=>l+ ## 0
-- /guard-np/      l+           ## 0
-- /conseq-np/     d=>l., a=>l+ ## 0
-- /guard-others/  l.           ## 0
-- /conseq-others/ d=>l., a=>l+ ## 0
