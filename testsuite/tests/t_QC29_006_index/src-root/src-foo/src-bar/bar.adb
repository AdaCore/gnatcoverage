pragma Ada_2012;
pragma Assertion_Policy (Check);

with Ada.Assertions;

function Bar return Boolean
is
  T : Boolean := True;
  F : Boolean := False;

  Res : Boolean := False;

  function MCDC_Violation (A, B : Boolean) return Boolean is
  begin
    if (A and then not B) or else A then
      return True;
    end if;
    return True;
  end MCDC_Violation;

begin
  Res := MCDC_Violation (T, F);
  Res := MCDC_Violation (F, F);

  begin
    pragma Assert (Res);
  exception
    when Ada.Assertions.Assertion_Error => null;
  end;

  return Res;
end Bar;
