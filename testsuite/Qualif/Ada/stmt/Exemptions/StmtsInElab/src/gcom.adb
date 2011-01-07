package body Gcom is

   --  Non-exempted subprogram body

   procedure Initialize is
   begin
      Initialized := True;   -- # init_body
   end;

begin
   if Auto_Init then

      --  Exempted sequence nested within a conditional, part of
      --  a generic package body elaboration sequence

      pragma Annotate                       -- # init_call
        (Xcov, Exempt_On, "auto init off"); -- # init_call
      Initialize;                           -- # init_call
      pragma Annotate (Xcov, Exempt_Off);   -- # init_call
   end if;
end;
