pragma Ada_2012;
pragma Assertion_Policy (Check);

with Ada.Assertions;

with Bar;
with Lib;

function Foo return Boolean
is
  T : Boolean := True;
  F : Boolean := False;
begin
  if not T then
    T := T and then T;
  end if;

  begin
    if T then
      pragma Assert (T);
      pragma Assert (T or else T or else T);
      pragma Assert (T and then Lib.Id (F));
    end if;
  exception
    when Ada.Assertions.Assertion_Error => null;
  end;

  return T;
end Foo;
