with Colors, Support; use Colors, Support;

package body Mycolors is

   procedure Check (Cc, Ref : Code) is
   begin
      Assert (Cc = Ref);
   end;

   procedure Do_Check_Red is
   begin
      Check (Cc => Code_For (Red), Ref => R);
   end;

   procedure Do_Check_Green is
   begin
      Check (Cc => Code_For (Green), Ref => G);
   end;

   procedure Do_Check_Blue is
   begin
      Check (Cc => Code_For (Blue), Ref => B);
   end;
end;
