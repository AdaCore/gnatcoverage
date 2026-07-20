procedure Main_2 is
begin
    pragma Annotate (1);
    pragma Annotate (Xcov, 1);
    pragma Annotate (Xcov, Exempt_On, "Valid" & " argument");
    pragma Annotate (Xcov, Exempt_On, 16#DEAD_BEEF#);
    pragma Annotate (Xcov, Exempt_On, No_Such_Arg => "foo");
    pragma Annotate (Xcov, Exempt_On, "too", "many", "args");
    pragma Annotate (Xcov, Exempt_On, 1.0);
    pragma Annotate (Xcov, Exempt_Decision_Outcome);
    pragma Annotate (Xcov, Exempt_Decision_Outcome, 1);
    pragma Annotate (Xcov, Exempt_Decision_Outcome, True, False);
    pragma Annotate (Xcov, Exempt_Decision_Condition);
    pragma Annotate (Xcov, Exempt_Decision_Condition, False);
    pragma Annotate (Xcov, Exempt_Decision_Condition, -1);
end Main_2;
