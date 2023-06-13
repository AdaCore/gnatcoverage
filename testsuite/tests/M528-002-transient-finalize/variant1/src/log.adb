with System;

--  A fake replacement for Put_Line so the original source constructs in
--  callers are generally preserved and we can exercise arbitrary an RTS
--  profile.

procedure Log (Status : String) is
   Outreg : System.Address;
   pragma Volatile (Outreg);
begin
   Outreg := Status'Address;
end;
