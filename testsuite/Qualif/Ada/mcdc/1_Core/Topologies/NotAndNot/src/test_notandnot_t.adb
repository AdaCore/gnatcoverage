with Support, Notandnot; use Support, Notandnot;

procedure Test_Notandnot_T is
begin
   Assert (F (False, False) = True);
end;

--# notandnot.adb
--  /eval(Stmt|Other)/ l! oF-
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l- s-
--  /returnValue/   l+ 0
--  /decl/   l+ 0
