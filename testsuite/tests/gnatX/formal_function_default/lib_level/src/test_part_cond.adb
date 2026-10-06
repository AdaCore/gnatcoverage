with Pkg; use Pkg;

procedure Test_Part_Cond is
   package Top_Left_Grid is new Grid;
begin
   if Top_Left_Grid.Point_Image ((0, 0)) /= '+' then
      raise Program_Error;
   elsif Top_Left_Grid.Point_Image ((0, 1)) /= '.' then
      raise Program_Error;
   end if;
end Test_Part_Cond;

--# pkg.ads
--
-- /default/  l! ## c!:"Self.X"
-- /override/ l- ## s-
