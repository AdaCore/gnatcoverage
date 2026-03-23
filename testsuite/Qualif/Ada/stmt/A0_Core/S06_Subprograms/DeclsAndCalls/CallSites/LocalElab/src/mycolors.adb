with Colors, Support; use Colors, Support;

package body Mycolors is

   procedure Do_Check_Red is
      package Mypack is end;
      package body Mypack is
      begin
         Assert (Code_For (Red) = R);
      end;
   begin
      null;
   end;

   procedure Do_Check_Green is
      package Mypack is end;
      package body Mypack is
      begin
         Assert (Code_For (Green) = G);
      end;
   begin
      null;
   end;

   procedure Do_Check_Blue is
      package Mypack is end;
      package body Mypack is
      begin
         Assert (Code_For (Blue) = B);
      end;
   begin
      null;
   end;
end;
