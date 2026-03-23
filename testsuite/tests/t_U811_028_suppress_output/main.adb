procedure Main is

   Bool1 : Boolean := False;
   pragma Volatile (Bool1);

   Bool2 : Boolean := True;
   pragma Volatile (Bool2);

   Res : Boolean;
   pragma Volatile (Res);

begin
   Res := Bool1 or else Bool2;
end Main;
