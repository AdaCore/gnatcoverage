package body A is
  procedure A_Proc (Dummy : Boolean) is
  begin
    pragma Annotate (Xcov, Exempt_On, "Justification");
    if Dummy and then not Dummy then
      pragma Assert (Dummy);
      declare
        Dummy_2 : Boolean := not Dummy and then Dummy;
      begin
        null;
      end;
    end if;
    pragma Annotate (Xcov, Exempt_Off);
  end A_Proc;
end A;
