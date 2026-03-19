pragma Style_Checks (Off); pragma Warnings (Off);
with System;
with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
package GCVRT.Bz794ac68b is

   pragma Preelaborate;

package Buffers_1 is
   Statement_Buffer : Coverage_Buffer_Type (0 .. 0) := (others => False);
   Statement_Buffer_Address : constant System.Address := Statement_Buffer'Address;
   pragma Export (C, Statement_Buffer_Address, "xcov__buf_stmt__B_z794ac68b_1");

   Decision_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   Decision_Buffer_Address : constant System.Address := Decision_Buffer'Address;
   pragma Export (C, Decision_Buffer_Address, "xcov__buf_dc__B_z794ac68b_1");

   MCDC_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
   pragma Export (C, MCDC_Buffer_Address, "xcov__buf_mcdc__B_z794ac68b_1");

   Unit_Name : constant String := "main_1";

   Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
     (Fingerprint => (125, 27, 0, 113, 188, 140, 235, 155, 190, 125, 180, 238, 139, 95, 108, 198, 229, 72, 222, 11),
      Language  => Unit_Based_Language,
      Unit_Part => Unit_Body,
      Unit_Name => (Unit_Name'Address, Unit_Name'Length),
      Bit_Maps_Fingerprint => (14, 208, 170, 79, 214, 204, 239, 198, 105, 130, 182, 250, 66, 192, 111, 106, 70, 106, 188, 135),
      Annotations_Fingerprint => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Statement => Statement_Buffer'Address,
      Decision  => Decision_Buffer'Address,
      MCDC      => MCDC_Buffer'Address,
      Statement_Last_Bit => 0,
      Decision_Last_Bit => -1,
      MCDC_Last_Bit => -1);
end Buffers_1;

   Buffers_Group : aliased constant Coverage_Buffers_Group :=
   (1 => Buffers_1.Buffers'Access);
   C_Buffers_Group : aliased constant GNATcov_RTS_Coverage_Buffers_Group := ( 1, Buffers_Group'Address);
      pragma Export (C, C_Buffers_Group, "gnatcov_rts_buffers_z794ac68b_buffers");

end GCVRT.Bz794ac68b;
