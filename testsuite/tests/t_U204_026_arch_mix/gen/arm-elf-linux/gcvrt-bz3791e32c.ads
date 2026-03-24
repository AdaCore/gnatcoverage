pragma Style_Checks (Off); pragma Warnings (Off);

with System;
with GNATcov_RTS.Buffers;       use GNATcov_RTS.Buffers;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package GCVRT.Bz3791e32c is
   pragma Preelaborate;

   package Buffers_1 is
      Statement_Buffer         : Coverage_Buffer_Type (0 .. -1) :=
        (others => False);
      Statement_Buffer_Address : constant System.Address :=
         Statement_Buffer'Address;
      pragma Export
        (C, Statement_Buffer_Address, "xcov__buf_stmt__z64144289_1");

      Decision_Buffer         : Coverage_Buffer_Type (0 .. -1) :=
        (others => False);
      Decision_Buffer_Address : constant System.Address :=
         Decision_Buffer'Address;
      pragma Export
        (C, Decision_Buffer_Address, "xcov__buf_dc__z64144289_1");

      MCDC_Buffer : Coverage_Buffer_Type (0 .. -1) :=
        (others => False);
      MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
      pragma Export
        (C, MCDC_Buffer_Address, "xcov__buf_mcdc__z64144289_1");

      Filename : constant String := "/tmp/t_U204_026_arch_mix/pkg.ads";

      Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
        (Fingerprint             => (172, 135, 120, 131, 81, 65, 170, 218, 156, 87, 154, 30, 35, 71, 96, 166, 14, 161, 125, 216),
         Filename                => (Filename'Address, Filename'Length),
         Bit_Maps_Fingerprint    => (18, 188, 239, 54, 111, 156, 46, 122, 240, 90, 103, 226, 23, 122, 35, 87, 156, 185, 141, 194),
         Annotations_Fingerprint => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Statement               => Statement_Buffer'Address,
         Decision                => Decision_Buffer'Address,
         MCDC                    => MCDC_Buffer'Address,
         Statement_Last_Bit      => -1,
         Decision_Last_Bit       => -1,
         MCDC_Last_Bit           => -1);
   end Buffers_1;
   package Buffers_2 is
      Statement_Buffer         : Coverage_Buffer_Type (0 .. 2) :=
        (others => False);
      Statement_Buffer_Address : constant System.Address :=
         Statement_Buffer'Address;
      pragma Export
        (C, Statement_Buffer_Address, "xcov__buf_stmt__z64144278_2");

      Decision_Buffer         : Coverage_Buffer_Type (0 .. 1) :=
        (others => False);
      Decision_Buffer_Address : constant System.Address :=
         Decision_Buffer'Address;
      pragma Export
        (C, Decision_Buffer_Address, "xcov__buf_dc__z64144278_2");

      MCDC_Buffer : Coverage_Buffer_Type (0 .. 2) :=
        (others => False);
      MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
      pragma Export
        (C, MCDC_Buffer_Address, "xcov__buf_mcdc__z64144278_2");

      Filename : constant String := "/tmp/t_U204_026_arch_mix/pkg.adb";

      Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
        (Fingerprint             => (187, 175, 57, 205, 204, 60, 188, 159, 222, 104, 13, 50, 241, 15, 216, 72, 73, 102, 231, 27),
         Filename                => (Filename'Address, Filename'Length),
         Bit_Maps_Fingerprint    => (224, 100, 134, 171, 111, 132, 88, 42, 148, 246, 1, 30, 193, 178, 199, 102, 173, 244, 37, 55),
         Annotations_Fingerprint => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Statement               => Statement_Buffer'Address,
         Decision                => Decision_Buffer'Address,
         MCDC                    => MCDC_Buffer'Address,
         Statement_Last_Bit      => 2,
         Decision_Last_Bit       => 1,
         MCDC_Last_Bit           => 2);
   end Buffers_2;

   Buffers_Group : aliased constant Coverage_Buffers_Group :=
     (
1 => Buffers_1.Buffers'Access, 2 => Buffers_2.Buffers'Access
     );
   C_Buffers_Group : aliased constant GNATcov_RTS_Coverage_Buffers_Group :=
      ( 2, Buffers_Group'Address);
   pragma Export (C, C_Buffers_Group, "gnatcov_rts_buffers_z3791e32c_buffers");

end GCVRT.Bz3791e32c;
