pragma Ada_2012;

with Support, Values; use Support, Values;

procedure Test_A is

   LA : Array_Type :=
     (1 => (A => True, B => True, Expr => <>),
      2 => (A => False, B => True, Expr => <>)
     );

begin
   Process (LA);
   for E of LA loop
      Assert (E.Expr = (E.A and then E.B));
   end loop;
end;

--# values.adb
--  /stmt/ l+ ## 0
--  /eval/ l! ## c!:"E.B"
