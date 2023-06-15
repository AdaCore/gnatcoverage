procedure Foo is
   type Void_PP is access Integer;

   N : Integer := 0;
   P : Void_PP;
   pragma Volatile (P);
begin
   N := 1;
   while N < 32 and then p.all /= 0 loop N := N + 1; end loop;

   null;

   --  Note: we used to have here:

   --  if N = 32 then
   --  N := N - 1;
   --  end if;

   --  But this is eliminated entirely at -O1 (because the value of
   --  N is never used), so use a NULL statement instead so that we
   --  don't get an additional junk warning for no cond branch found
   --  for the IF statement.

end Foo;
