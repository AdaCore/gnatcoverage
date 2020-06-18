with Support; use Support;
with Sys; use Sys;

procedure Test_Full is
   X : Boolean := False;
begin
   Assert (And_Or_C0(Cond => True, A => True, B => True, C => X) = True);
   Assert (And_Or_C0(Cond => True, A => False, B => True, C => X) = False);
   Assert (And_Or_C0(Cond => True, A => True, B => False, C => X) = False);

   -- Just make sure Cond is covered
   Assert (And_Or_C0(Cond => False, A => X, B => X, C => True) = True);

   --

   Assert (C_Or_Or0(Cond => False, A => False, B => False, C => X) = False);
   Assert (C_Or_Or0(Cond => False, A => True, B => X, C => X) = True);
   Assert (C_Or_Or0(Cond => False, A => False, B => True, C => X) = True);

   -- Just make sure Cond is covered
   Assert (C_Or_Or0(Cond => True, A => X, B => X, C => True) = True);

end;

--# sys.adb
--  /eval/ l+ ## 0
