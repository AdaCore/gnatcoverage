package Ops is

   -- Expose various 'Op'erators that will all work on
   -- a common data structure:

   type Opdata is record
      Nops_Done : Integer := 0;
   end record;

   -- Visible subunit here --

   package Vsub is
      procedure Op (Opd: in out Opdata);
   end;

   procedure Do_Ops (V, P, I: Boolean; Opd: in out Opdata);
   -- Call into the Visible/Private/Internal operators, as
   -- queried by V/P/I values respectively

   procedure Touch (Opd: in out Opdata);
   -- Common facility for all operators

private

   -- Private subunit here --

   package Psub is
      procedure Op (Opd: in out Opdata);
   end;

end;
