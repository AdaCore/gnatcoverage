procedure Main is
    X : Natural := 0;
    pragma volatile (X);
begin
    X := 1;
end Main;
