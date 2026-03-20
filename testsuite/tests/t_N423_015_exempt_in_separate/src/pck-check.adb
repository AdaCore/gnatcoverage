
separate (Pck)
procedure Check (Valid : Boolean) is
begin
   if Valid then
      Valids := Valids + 1; -- # valids
   else
      pragma Annotate (Xcov, Exempt_On, "Invalids unexpected"); -- # invalids
      Invalids := Invalids + 1;                                 -- # invalids_inc
      pragma Annotate (Xcov, Exempt_Off);                       -- # invalids
   end if;
end;
