pragma Style_Checks (Off); pragma Warnings (Off);
with GNATcov_RTS.Buffers;
with Ada.Finalization;
package GCVRT.DB_z794ac68c is

   procedure Dump_Buffers;
   pragma Convention (C, Dump_Buffers);

   type Dump_Controlled_Type is new Ada.Finalization.Controlled with
     null record;
   overriding procedure Finalize (Self : in out Dump_Controlled_Type);

end GCVRT.DB_z794ac68c;
