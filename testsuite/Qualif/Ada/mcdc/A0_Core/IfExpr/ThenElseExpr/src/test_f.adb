with Support; use Support;
with Sys; use Sys;

procedure Test_F is
   X : Boolean := False;
begin
   Assert (And_Or_C0(Cond => True, A => False, B => X, C => X) = False);
   Assert (And_Or_C0(Cond => True, A => True, B => False, C => X) = False);

   -- Just make sure Cond is covered
   Assert (And_Or_C0(Cond => False, A => X, B => X, C => True) = True);

   --

   Assert (C_Or_Or0(Cond => False, A => False, B => False, C => X) = False);

   -- Just make sure Cond is covered
   Assert (C_Or_Or0(Cond => True, A => X, B => X, C => True) = True);
end;

--# sys.adb
--  /eval/ l! ## eT-:"A"
