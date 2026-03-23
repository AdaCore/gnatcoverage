with A;
with Foo;
with Lib;

procedure Main is
  T     : Boolean := True;
  F     : Boolean := False;
begin
  if Lib.Id (T) or else F then
    T := T and then True;
  end if;

  if F and then Foo then
    F := F and then False;
  end if;

  A.A_Proc (True);
end Main;
