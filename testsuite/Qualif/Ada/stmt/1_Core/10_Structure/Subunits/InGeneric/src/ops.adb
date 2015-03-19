package body Ops is

   procedure Touch (Opd: in out Opdata) is
   begin
      Opd.Nops_Done := Opd.Nops_Done + 1; -- # touch
   end Touch;

end Ops;
