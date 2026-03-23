with Colors, Support; use Colors, Support;

package body Mycolors is
   procedure Do_Check_Red is
      type Holder is record
         Cc : Code := Code_For (Red);
      end record;
      H : Holder;
   begin
      Assert (H.Cc = R);
   end;

   procedure Do_Check_Green is
      type Holder is record
         Cc : Code := Code_For (Green);
      end record;
      H : Holder;
   begin
      Assert (H.Cc = G);
   end;

   procedure Do_Check_Blue is
      type Holder is record
         Cc : Code := Code_For (Blue);
      end record;
      H : Holder;
   begin
      Assert (H.Cc = B);
   end;
end;
