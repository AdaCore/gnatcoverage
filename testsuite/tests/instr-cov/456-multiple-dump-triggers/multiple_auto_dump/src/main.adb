procedure Main is

   procedure Increment (J : in out Integer)
   is
   begin
      J := J + 1;
   end Increment;

   I : Integer := 1;
begin
   if 1 = I + 1 then
      I := I+ 1;
   end if;
   pragma Annotate (Xcov, Dump_Buffers, "manual_dump");
   pragma Annotate (Xcov, Reset_Buffers);

   Increment (I);
   I := I + 42;
   I := I + 1;
end Main;
