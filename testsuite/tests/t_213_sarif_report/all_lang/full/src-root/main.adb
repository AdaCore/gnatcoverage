with A;
with Foo;
with Lib;

procedure Main
is

  function C_Func (A : Integer; B : Integer) return Integer;
  pragma Import (C, C_Func, "c_func");

  T       : Boolean := True;
  F       : Boolean := False;
  Rec     : A.T := A.Create;
  Dummy_I : Integer := C_Func (1, 2);

begin

  if Lib.Id (T) or else F then
    T := T and then True;
  end if;

  if F and then Foo then
    F := F and then False;
  end if;

  pragma Annotate (Xcov, Exempt_On, "Test");
  if not Foo then
    T := not F;
  end if;
  pragma Annotate (Xcov, Exempt_Off);

  A.A_Proc (True);

end Main;
