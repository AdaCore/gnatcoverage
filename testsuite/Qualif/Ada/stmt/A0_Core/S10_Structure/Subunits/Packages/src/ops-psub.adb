separate (Ops)
package body Psub is
   procedure Op (Opd: in out Opdata) is
   begin
      Touch (Opd); -- # psub
   end;
end;
