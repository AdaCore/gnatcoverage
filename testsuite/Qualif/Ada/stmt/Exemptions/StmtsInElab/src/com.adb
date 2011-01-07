
package body Com is

   --  Non-exempted subprogram body

   procedure Initialize is
   begin
      Current_State := Idle; -- # init_body
      Initialized := True;   -- # init_body
   end;

begin

   if Auto_Initialize then

      --  Exempted sequence nested within a conditional, part of
      --  a non-generic package body elaboration sequence

      pragma Annotate                       -- # init_call
        (Xcov, Exempt_On, "auto init off"); -- # init_call
      Initialize;                           -- # init_call
      pragma Annotate (Xcov, Exempt_Off);   -- # init_call
   end if;
end;
