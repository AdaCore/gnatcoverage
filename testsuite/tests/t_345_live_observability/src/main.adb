with Ada.Text_IO; use Ada.Text_IO;
with GCVRT.Gen.Observe; use GCVRT.Gen.Observe;
with Pkg;

procedure Main is
begin
   --  No statement should be covered yet here.
   Put_Line ("First: " & Positive'Image (Sum_Buffer_Bits));

   Pkg.Foo (False);

   --  2 statements were executed since last call.
   --  (if stmt + False Put_Line)
   Put_Line ("Second: " & Positive'Image (Sum_Buffer_Bits));

   Pkg.Foo (True);

   --  2 statements were executed since last call.
   --  (if stmt is already covered, + 2 Put_Lines)
   Put_Line ("Third: " & Positive'Image (Sum_Buffer_Bits));

   --  No change since last call
   Put_Line ("Third-bis: " & Positive'Image (Sum_Buffer_Bits));

   Pkg.Bar (42);

   --  2 more statements covered
   --  (Integer declaration, Put_Line)
   Put_Line ("Fourth: " & Positive'Image (Sum_Buffer_Bits));
end Main;
