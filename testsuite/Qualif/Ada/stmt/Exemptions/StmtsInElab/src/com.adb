package body Com is
   procedure Initialize is
   begin
      Current_State := Idle; -- # init_body
      Initialized := True;   -- # init_body
   end;

begin
   if Auto_Initialize then
      pragma Annotate                       -- # init_call
        (Xcov, Exempt_On, "auto init off"); -- # init_call
      Initialize;                           -- # init_call
      pragma Annotate (Xcov, Exempt_Off);   -- # init_call
   end if;
end;
