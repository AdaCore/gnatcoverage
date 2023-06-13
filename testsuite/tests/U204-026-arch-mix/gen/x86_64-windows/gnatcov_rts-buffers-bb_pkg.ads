pragma Style_Checks (Off); pragma Warnings (Off);
with Interfaces.C; use Interfaces.C;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
package GNATcov_RTS.Buffers.BB_pkg is

   pragma Preelaborate;

   Statement_Buffer : Coverage_Buffer_Type (0 .. 2) := (others => False);
   Statement_Buffer_Address : constant System.Address := Statement_Buffer'Address;
   pragma Export (C, Statement_Buffer_Address, "xcov__buf_stmt__B_pkg");

   Decision_Buffer : Coverage_Buffer_Type (0 .. 1) := (others => False);
   Decision_Buffer_Address : constant System.Address := Decision_Buffer'Address;
   pragma Export (C, Decision_Buffer_Address, "xcov__buf_dc__B_pkg");

   MCDC_Buffer : Coverage_Buffer_Type (0 .. 2) := (others => False);
   MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
   pragma Export (C, MCDC_Buffer_Address, "xcov__buf_mcdc__B_pkg");

   Unit_Name : constant String := "pkg";
   Project_Name : constant String := "";

   Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
     (Fingerprint => (169, 72, 99, 183, 162, 15, 92, 242, 46, 209, 243, 55, 176, 82, 17, 62, 254, 76, 153, 48),
      Language  => Unit_Based_Language,
      Unit_Part => Unit_Body,
      Unit_Name => (Unit_Name'Address, Unit_Name'Length),
      Project_Name => (Project_Name'Address, Project_Name'Length),
      Bit_Maps_Fingerprint => (161, 27, 188, 138, 241, 245, 9, 150, 143, 0, 181, 204, 90, 152, 68, 88, 187, 222, 106, 42),
      Statement => Statement_Buffer'Address,
      Decision  => Decision_Buffer'Address,
      MCDC      => MCDC_Buffer'Address,
      Statement_Last_Bit => 2,
      Decision_Last_Bit => 1,
      MCDC_Last_Bit => 2);

   Buffers_Group : aliased constant Coverage_Buffers_Group := (1 => Buffers'Access);
   C_Buffers_Group : aliased constant GNATcov_RTS_Coverage_Buffers_Group := (1, Buffers_Group'Address);
      pragma Export (C, C_Buffers_Group, "gnatcov_rts_buffers_B_pkg_buffers");

end GNATcov_RTS.Buffers.BB_pkg;
