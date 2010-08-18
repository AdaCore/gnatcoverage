with Support, Goto_Next;  use Support, Goto_Next;

procedure Test_Id_Full is
   K : constant Integer := 5;
begin
   Assert (Goto_Next.Identity (K) = K);
end;

--# goto_next.adb
--  /Call/  l+ 0
