procedure Main_2 is
begin
    pragma Annotate (1);
    pragma Annotate (Xcov, 1);
    pragma Annotate (Xcov, Exempt_On, "Valid" & " argument");
    pragma Annotate (Xcov, Exempt_On, 16#DEAD_BEEF#);
end Main_2;
