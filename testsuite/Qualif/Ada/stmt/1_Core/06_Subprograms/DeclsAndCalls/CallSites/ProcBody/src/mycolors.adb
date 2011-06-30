with Colors, Support; use Colors, Support;

package body Mycolors is
   procedure Do_Check_Red is
   begin
      Assert (Code_For (Red) = R);
   end;

   procedure Do_Check_Green is
   begin
      Assert (Code_For (Green) = G);
   end;

   procedure Do_Check_Blue is
   begin
      Assert (Code_For (Blue) = B);
   end;
end;
