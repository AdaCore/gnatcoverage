package body Ops is

   package body Vsub is separate;
   package body Psub is separate;

   procedure Touch (Opd: in out Opdata) is
   begin
      Opd.Nops_Done := Opd.Nops_Done + 1; -- # touch
   end;

   -- Internal subunit here --
   package Isub is
      procedure Op (Opd: in out Opdata);
   end;

   package body Isub is separate;

   procedure Do_Ops (V, P, I : Boolean; Opd: in out Opdata) is
   begin
      if V then         -- # doops
         Vsub.Op (Opd); -- # vsub
      end if;
      if P then         -- # doops
         Psub.Op (Opd); -- # psub
      end if;
      if I then         -- # doops
         Isub.Op (Opd); -- # isub
      end if;
   end;

end;
