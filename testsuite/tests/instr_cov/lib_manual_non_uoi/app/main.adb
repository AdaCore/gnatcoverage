with Covered;

procedure Main is
   R : Integer;
begin
   R := Covered.Double (21);
   if R = 42 then
      null;
   end if;
   pragma Annotate (Xcov, Dump_Buffers);
end Main;
