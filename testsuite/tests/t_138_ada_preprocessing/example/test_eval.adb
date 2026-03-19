with VM; use VM;

procedure Test_Eval is
   Program : constant Program_Type :=
     (1 => (Kind => Clone),
      2 => (Kind => Branch, Jump_Dest => 4),
      3 => (Kind => Halt),
      4 => (Kind => Push_Lit, Push_Value => -1),
      5 => (Kind => Add),
      6 => (Kind => Jump, Jump_Dest => 1));
begin
   if Eval (Program, 5, (1 => 0)) /= 0 then
      raise Program_Error;
   end if;
end Test_Eval;
