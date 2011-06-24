separate (Ops)
package body Vsub is
   procedure Op (Opd: in out Opdata) is
   begin
      Touch (Opd); -- # vsub
   end;
end;
