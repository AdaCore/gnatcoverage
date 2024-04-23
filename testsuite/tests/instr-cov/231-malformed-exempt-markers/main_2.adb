procedure Main_2 is
begin
    pragma Annotate (1);
    pragma Annotate (Xcov, 1);
    pragma Annotate (Xcov, Exempt_On, "Invalid" & " argument");
end Main_2;
