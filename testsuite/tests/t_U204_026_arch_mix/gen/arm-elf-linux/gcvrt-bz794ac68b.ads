pragma Style_Checks (Off); pragma Warnings (Off);

with System;
with GNATcov_RTS.Buffers;       use GNATcov_RTS.Buffers;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package GCVRT.Bz794ac68b is
   pragma Preelaborate;

   package Buffers_1 is
      Statement_Buffer         : Coverage_Buffer_Type (0 .. 0) :=
        (others => False);
      Statement_Buffer_Address : constant System.Address :=
         Statement_Buffer'Address;
      pragma Export
        (C, Statement_Buffer_Address, "xcov__buf_stmt__z021e4ae1_1");

      Decision_Buffer         : Coverage_Buffer_Type (0 .. -1) :=
        (others => False);
      Decision_Buffer_Address : constant System.Address :=
         Decision_Buffer'Address;
      pragma Export
        (C, Decision_Buffer_Address, "xcov__buf_dc__z021e4ae1_1");

      MCDC_Buffer : Coverage_Buffer_Type (0 .. -1) :=
        (others => False);
      MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
      pragma Export
        (C, MCDC_Buffer_Address, "xcov__buf_mcdc__z021e4ae1_1");

      Filename : constant String := "/tmp/t_U204_026_arch_mix/main_1.adb";

      Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
        (Fingerprint             => (106, 13, 66, 61, 138, 186, 69, 82, 48, 55, 131, 49, 211, 239, 150, 134, 208, 146, 21, 201),
         Filename                => (Filename'Address, Filename'Length),
         Bit_Maps_Fingerprint    => (228, 252, 99, 232, 172, 243, 155, 56, 89, 239, 143, 211, 57, 206, 9, 239, 178, 154, 99, 251),
         Annotations_Fingerprint => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Statement               => Statement_Buffer'Address,
         Decision                => Decision_Buffer'Address,
         MCDC                    => MCDC_Buffer'Address,
         Statement_Last_Bit      => 0,
         Decision_Last_Bit       => -1,
         MCDC_Last_Bit           => -1);
   end Buffers_1;

   Buffers_Group : aliased constant Coverage_Buffers_Group :=
     (
1 => Buffers_1.Buffers'Access
     );
   C_Buffers_Group : aliased constant GNATcov_RTS_Coverage_Buffers_Group :=
      ( 1, Buffers_Group'Address);
   pragma Export (C, C_Buffers_Group, "gnatcov_rts_buffers_z794ac68b_buffers");

end GCVRT.Bz794ac68b;
