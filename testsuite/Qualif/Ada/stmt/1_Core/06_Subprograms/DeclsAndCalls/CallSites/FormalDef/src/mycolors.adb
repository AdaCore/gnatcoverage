with Colors, Support; use Colors, Support;

package body Mycolors is

   procedure Do_Check_Red is
      procedure Check (Cc : Code := Code_For (Red); Ref : Code := R) is
      begin
         Assert (Cc = Ref);
      end;
   begin
      Check;
   end;

   procedure Do_Check_Green is
      procedure Check (Cc : Code := Code_For (Green); Ref : Code := G) is
      begin
         Assert (Cc = Ref);
      end;
   begin
      Check;
   end;

   procedure Do_Check_Blue is
      procedure Check (Cc : Code := Code_For (Blue); Ref : Code := B) is
      begin
         Assert (Cc = Ref);
      end;
   begin
      Check;
   end;
end;
