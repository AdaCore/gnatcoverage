with Pkg; use Pkg;

procedure Test_Override is
   package Bottom_Right is new Grid (Bottom_Right_Is_Center);
begin
   if Bottom_Right.Point_Image ((Width - 1, Height - 1)) /= '+' then
      raise Program_Error;
   end if;
end Test_Override;

--# pkg.ads
--
-- /default/  l- ## s-
-- /override/ l+ ## 0
