separate (Ops)
package body Isub is
   procedure Op (Opd: in out Opdata) is
   begin
      Touch (Opd); -- # isub
   end;
end;
