pragma Ada_2012;

with Lib1;
with Lib2;

procedure Main is

   procedure Increment (J : in out Integer)
   is
   begin
      J := J + 1;
   end Increment;

   I : Integer := 1;
begin
    -- The only call that should not count as a violation when never executed
    -- is that of the dump buffers procedure.
   if 1 = I + 1 then
      pragma Annotate (Xcov, Dump_Buffers);
      I := I+ 1;
   end if;

   Increment (I);
   I := I + Lib1.Foo;
   I := I + Lib2.Bar;
   Increment (I);
end Main;
