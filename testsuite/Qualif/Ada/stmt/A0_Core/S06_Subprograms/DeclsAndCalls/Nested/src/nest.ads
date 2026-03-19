package Nest is

   type Cdata is record
      Nops : Integer := 0;
   end record;

   procedure Check
     (Lfun, Lproc, Pfun, Pproc, Indirect : Boolean; Cd : access Cdata);

end;
