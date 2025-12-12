with Pkg; use Pkg;

procedure Test_Full is
   package Top_Left_Grid is new Grid;
begin
   if Top_Left_Grid.Point_Image ((0, 0)) /= '+' then
      raise Program_Error;
   elsif Top_Left_Grid.Point_Image ((1, 0)) /= '.' then
      raise Program_Error;
   elsif Top_Left_Grid.Point_Image ((0, 1)) /= '.' then
      raise Program_Error;
   end if;
end Test_Full;

--# pkg.ads
--
-- /default/  l+ ## 0
-- /override/ l- ## s-
