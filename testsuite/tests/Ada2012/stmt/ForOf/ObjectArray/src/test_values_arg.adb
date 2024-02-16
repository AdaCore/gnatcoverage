pragma Ada_2012;

with Values.ARG, Support; use Values, Support;

procedure Test_Values_ARG is
   LA : Array_Type := (others => (X => 5));
begin
   ARG.Do_Loop_Over (LA);
   for E of LA loop
      Assert (E.X = 6);
   end loop;
end;

--# values-arg.adb
--  /stmt/    l+ ## 0
--  /loop_op/ l+ ## 0
