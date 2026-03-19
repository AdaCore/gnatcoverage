with Mylib;

procedure Prog is
begin
   if Mylib.Foo (True) = 3 then
      raise Program_Error;
   end if;
end Prog;
