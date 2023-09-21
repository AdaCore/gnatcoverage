pragma Style_Checks (Off); pragma Warnings (Off);
with Interfaces.C; use Interfaces.C;
with System;
with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
package GCVRT.Bz3791e32c is

   pragma Preelaborate;

package Buffers_1 is
   Statement_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   Statement_Buffer_Address : constant System.Address := Statement_Buffer'Address;
   pragma Export (C, Statement_Buffer_Address, "xcov__buf_stmt__S_z3791e32c_1");

   Decision_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   Decision_Buffer_Address : constant System.Address := Decision_Buffer'Address;
   pragma Export (C, Decision_Buffer_Address, "xcov__buf_dc__S_z3791e32c_1");

   MCDC_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
   pragma Export (C, MCDC_Buffer_Address, "xcov__buf_mcdc__S_z3791e32c_1");

   Unit_Name : constant String := "pkg";

   Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
     (Fingerprint => (172, 135, 120, 131, 81, 65, 170, 218, 156, 87, 154, 30, 35, 71, 96, 166, 14, 161, 125, 216),
      Language  => Unit_Based_Language,
      Unit_Part => Unit_Spec,
      Unit_Name => (Unit_Name'Address, Unit_Name'Length),
      Bit_Maps_Fingerprint => (18, 188, 239, 54, 111, 156, 46, 122, 240, 90, 103, 226, 23, 122, 35, 87, 156, 185, 141, 194),
      Statement => Statement_Buffer'Address,
      Decision  => Decision_Buffer'Address,
      MCDC      => MCDC_Buffer'Address,
      Statement_Last_Bit => -1,
      Decision_Last_Bit => -1,
      MCDC_Last_Bit => -1);
end Buffers_1;

package Buffers_2 is
   Statement_Buffer : Coverage_Buffer_Type (0 .. 2) := (others => False);
   Statement_Buffer_Address : constant System.Address := Statement_Buffer'Address;
   pragma Export (C, Statement_Buffer_Address, "xcov__buf_stmt__B_z3791e32c_2");

   Decision_Buffer : Coverage_Buffer_Type (0 .. 1) := (others => False);
   Decision_Buffer_Address : constant System.Address := Decision_Buffer'Address;
   pragma Export (C, Decision_Buffer_Address, "xcov__buf_dc__B_z3791e32c_2");

   MCDC_Buffer : Coverage_Buffer_Type (0 .. 2) := (others => False);
   MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
   pragma Export (C, MCDC_Buffer_Address, "xcov__buf_mcdc__B_z3791e32c_2");

   Unit_Name : constant String := "pkg";

   Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
     (Fingerprint => (169, 72, 99, 183, 162, 15, 92, 242, 46, 209, 243, 55, 176, 82, 17, 62, 254, 76, 153, 48),
      Language  => Unit_Based_Language,
      Unit_Part => Unit_Body,
      Unit_Name => (Unit_Name'Address, Unit_Name'Length),
      Bit_Maps_Fingerprint => (161, 27, 188, 138, 241, 245, 9, 150, 143, 0, 181, 204, 90, 152, 68, 88, 187, 222, 106, 42),
      Statement => Statement_Buffer'Address,
      Decision  => Decision_Buffer'Address,
      MCDC      => MCDC_Buffer'Address,
      Statement_Last_Bit => 2,
      Decision_Last_Bit => 1,
      MCDC_Last_Bit => 2);
end Buffers_2;

   Buffers_Group : aliased constant Coverage_Buffers_Group :=
   (1 => Buffers_1.Buffers'Access,
2 => Buffers_2.Buffers'Access);
   C_Buffers_Group : aliased constant GNATcov_RTS_Coverage_Buffers_Group := ( 2, Buffers_Group'Address);
      pragma Export (C, C_Buffers_Group, "gnatcov_rts_buffers_z3791e32c_buffers");

end GCVRT.Bz3791e32c;
