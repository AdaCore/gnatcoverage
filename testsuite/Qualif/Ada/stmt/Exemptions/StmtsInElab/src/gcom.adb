package body Gcom is
   procedure Initialize is
   begin
      Initialized := True;   -- # init_body
   end;

begin
   if Auto_Init then
      pragma Annotate                       -- # init_call
        (Xcov, Exempt_On, "auto init off"); -- # init_call
      Initialize;                           -- # init_call
      pragma Annotate (Xcov, Exempt_Off);   -- # init_call
   end if;
end;
