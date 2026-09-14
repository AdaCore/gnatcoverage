with Ada.Text_IO; use Ada.Text_IO;

with GCVRT.Main_Obs.Observe;

procedure Main_Obs is
   function Foo (X : Integer) return Integer;
   pragma Import (C, Foo, "foo");

   Before, After : Natural;
begin
   Before := GCVRT.Main_Obs.Observe.Sum_Buffer_Bits;
   if Foo (1) /= 2 then
      raise Program_Error;
   end if;
   After := GCVRT.Main_Obs.Observe.Sum_Buffer_Bits;
   Put_Line ("Before:" & Natural'Image (Before));
   Put_Line ("After:" & Natural'Image (After));
end Main_Obs;
