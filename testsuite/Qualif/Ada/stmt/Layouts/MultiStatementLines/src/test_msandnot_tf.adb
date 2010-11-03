with Support, Ms_Lines; use Support, Ms_Lines;

--  Call two provided services, each featuring a mutlistatements line. Verify
--  that partial coverage is reported on these lines as soon as at least one
--  of the statements is covered, even when all.

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
--  /doAndNot/  l! s!
--  /setCarry/  l+ 0
--  /doHalfAdd/ l! s!
