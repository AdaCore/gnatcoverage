with Ada.Text_IO; use Ada.Text_IO;
with GCVRT.Gen.Observe; use GCVRT.Gen.Observe;

procedure Main is
   N : Integer;
begin
   --  Two statements executed
   --  - the one for the declaration of N
   --  - the one for this specific statement
   Put_Line ("First: " & Positive'Image (Sum_Buffer_Bits));

   N := 5;

   -- 2 two statements were executed since last call.
   Put_Line ("Second: " & Positive'Image (Sum_Buffer_Bits));

   if N > 3 then
      Put_Line ("Foo");
      Put_Line ("Bar");
   else
      Put_Line ("Baz");
   end if;

   -- 4 more statements were executed (including the if stmt).
   Put_Line ("Third: " & Positive'Image (Sum_Buffer_Bits));
end Main;
