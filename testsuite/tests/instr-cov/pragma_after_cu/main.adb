with Raise_Error;
with Silent_Last_Chance;

procedure Main is
begin
   Raise_Error;
exception
   when Program_Error =>
      null;
end Main;
