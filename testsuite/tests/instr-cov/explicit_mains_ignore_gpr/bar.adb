with Foo;

procedure Bar is
    X : Natural := 0;
    pragma volatile (X);
begin
    if X > 0 then
        Foo;
    end if;
    X := 1;
end Bar;
