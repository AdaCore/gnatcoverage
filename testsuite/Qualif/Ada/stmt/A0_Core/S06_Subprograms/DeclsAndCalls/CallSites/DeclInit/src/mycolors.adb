with Colors, Support; use Colors, Support;

package body Mycolors is
   procedure Do_Check_Red is
      Cc : Code := Code_For (Red);
   begin
      Assert (Cc = R);
   end;

   procedure Do_Check_Green is
      Cc : Code := Code_For (Green);
   begin
      Assert (Cc = G);
   end;

   procedure Do_Check_Blue is
      Cc : Code := Code_For (Blue);
   begin
      Assert (Cc = B);
   end;
end;
