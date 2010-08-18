with Support, Ms_Lines; use Support, Ms_Lines;

procedure Test_MsAndNot_TF is
   E : Boolean;
   S, C : Boolean;
begin
   Eval_And_Not (True, False, E);
   Assert (E = True);

   Eval_Half_Add (True, False, S, C);
   Assert (S = True and then C = False);
end;

--# ms_lines.adb
--  /setCarry/ l+ 0
--  /Partial/  l! s!
