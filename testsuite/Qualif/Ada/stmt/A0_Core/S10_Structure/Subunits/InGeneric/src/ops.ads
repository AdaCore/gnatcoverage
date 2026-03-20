package Ops is

   type Opdata is record
      Nops_Done : Integer := 0;
   end record;

   procedure Touch (Opd: in out Opdata);

end Ops;
