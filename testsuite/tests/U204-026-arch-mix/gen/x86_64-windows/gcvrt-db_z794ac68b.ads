pragma Style_Checks (Off); pragma Warnings (Off);
with GNATcov_RTS.Buffers;
with Ada.Finalization;
package GCVRT.DB_z794ac68b is

   pragma Preelaborate;

   pragma No_Tagged_Streams;

   procedure Dump_Buffers;
   pragma Convention (C, Dump_Buffers);

   type Dump_Controlled_Type is new
     Ada.Finalization.Limited_Controlled
     with null record;
   procedure Finalize (Self : in out Dump_Controlled_Type);

end GCVRT.DB_z794ac68b;
