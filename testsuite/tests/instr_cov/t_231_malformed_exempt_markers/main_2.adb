procedure Main_2 is
begin
    pragma Annotate (1);
    pragma Annotate (Xcov, 1);
    pragma Annotate (Xcov, Exempt_On, "Valid" & " argument");
    pragma Annotate (Xcov, Exempt_On, 16#DEAD_BEEF#);
    pragma Annotate (Xcov, Exempt_On, No_Such_Arg => "foo");
    pragma Annotate (Xcov, Exempt_On, "too", "many", "args");
end Main_2;
