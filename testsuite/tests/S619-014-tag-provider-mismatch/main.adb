with Generic_Hello;

procedure Main is
   procedure Hello_World is new Generic_Hello ("world");
   procedure Hello_There is new Generic_Hello ("there");

   S : constant String := "hello";
begin
   Hello_World;
   if S = "never-going-to-be-true" then
      Hello_There;
   end if;
end Main;
