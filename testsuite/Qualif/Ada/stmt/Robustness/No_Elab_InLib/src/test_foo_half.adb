--  This main program is linked with a library which encompasses
--  4 units: Foo, Bar and Klunk. Klunk isn't withed on purpose.

with Foo, Bar;

procedure Test_Foo_Half is
   K : Foo.Myint;
   pragma Volatile (K);
begin
   K := 34;
end;

--  Foo contains declarations and a subprogram. The declarations don't
--  generate code and the subprogram isn't referenced. The unit is withed
--  here, so we expect no violation on the declaration SCOs (no code), and
--  violations on the subprogram statement SCOs.

--# ../mylib/foo.ads
-- /decl/ l. ## 0

--# ../mylib/foo.adb
-- /body/ l- ## s-


--  Bar contains declarations only, no code at all. The unit is withed here
--  so we don't expect violations on the declaration SCOs.

--# ../mylib/bar.ads
-- /decl/ l. ## 0


--  Klunk contains declarations only, no code at all. The unit is not even
--  withed here so we expect violations on the declaration SCOs, even though
--  they are no code, to notify that coverage was requested for a unit not
--  part of the executable closure.

--# ../mylib/klunk.ads
-- /decl/ l- ## s-
