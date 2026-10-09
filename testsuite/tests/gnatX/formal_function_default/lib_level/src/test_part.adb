with Pkg; use Pkg;

procedure Test_Part is
   package Top_Left_Grid is new Grid;
begin
   if Top_Left_Grid.Point_Image ((0, 0)) /= '+' then
      raise Program_Error;
   end if;
end Test_Part;

--# pkg.ads
--
-- /default/  l! ## eF-
-- /override/ l- ## s-
