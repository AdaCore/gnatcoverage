pragma Style_Checks (Off); pragma Warnings (Off);

with System;
with GNATcov_RTS.Buffers;       use GNATcov_RTS.Buffers;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package GCVRT.Bz794ac68c is
   pragma Preelaborate;

   package Buffers_1 is
      Statement_Buffer         : Coverage_Buffer_Type (0 .. 0) :=
        (others => False);
      Statement_Buffer_Address : constant System.Address :=
         Statement_Buffer'Address;
      pragma Export
        (C, Statement_Buffer_Address, "xcov__buf_stmt__z460aa9e2_1");

      Decision_Buffer         : Coverage_Buffer_Type (0 .. -1) :=
        (others => False);
      Decision_Buffer_Address : constant System.Address :=
         Decision_Buffer'Address;
      pragma Export
        (C, Decision_Buffer_Address, "xcov__buf_dc__z460aa9e2_1");

      MCDC_Buffer : Coverage_Buffer_Type (0 .. -1) :=
        (others => False);
      MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
      pragma Export
        (C, MCDC_Buffer_Address, "xcov__buf_mcdc__z460aa9e2_1");

      Filename : constant String := "/tmp/t_U204_026_arch_mix/main_2.adb";

      Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
        (Fingerprint             => (91, 111, 84, 47, 59, 170, 241, 201, 183, 106, 198, 225, 186, 129, 94, 99, 218, 251, 32, 207),
         Filename                => (Filename'Address, Filename'Length),
         Bit_Maps_Fingerprint    => (58, 157, 18, 8, 23, 166, 32, 170, 215, 100, 28, 213, 238, 91, 76, 213, 203, 70, 164, 128),
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
   pragma Export (C, C_Buffers_Group, "gnatcov_rts_buffers_z794ac68c_buffers");

end GCVRT.Bz794ac68c;
