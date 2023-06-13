with Pkg;

--  This is prj2's foo.adb

procedure Foo is
begin
   for I in 1 .. 10 loop
      Pkg.Do_Nothing;
   end loop;
end Foo;
