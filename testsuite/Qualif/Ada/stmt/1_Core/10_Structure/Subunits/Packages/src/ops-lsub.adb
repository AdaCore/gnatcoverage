separate (Ops)
package body Lsub is
   procedure Op (Opd: in out Opdata) is
   begin
      Touch (Opd); -- # lsub
   end;
end;
