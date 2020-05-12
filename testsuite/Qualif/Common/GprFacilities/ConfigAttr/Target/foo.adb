procedure Foo is
   I : Integer := 0;
   pragma Volatile (I);
begin
   if I > 0 then
      I := 1;
   end if;
end Foo;
