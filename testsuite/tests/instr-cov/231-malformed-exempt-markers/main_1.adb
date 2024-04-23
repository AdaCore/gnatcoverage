procedure Main_1
is
    T : Boolean := True;
begin
    pragma Annotate (Xcov, Exempt_On);
    if not T then
        null;
    end if;
    pragma Annotate (Xcov, Exempt_Off, "Superfluous argument");
    pragma Annotate (Xcov, AAA, "Unrecognized annotation");
    pragma Annotate (Xcov);
    pragma Annotate (Test);  --  No warning from gnatcov
end Main_1;
