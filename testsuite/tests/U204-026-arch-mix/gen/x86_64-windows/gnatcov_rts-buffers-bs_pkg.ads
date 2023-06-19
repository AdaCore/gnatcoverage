pragma Style_Checks (Off); pragma Warnings (Off);
with Interfaces.C; use Interfaces.C;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
package GNATcov_RTS.Buffers.BS_pkg is

   pragma Preelaborate;

   Statement_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   Statement_Buffer_Address : constant System.Address := Statement_Buffer'Address;
   pragma Export (C, Statement_Buffer_Address, "xcov__buf_stmt__S_pkg");

   Decision_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   Decision_Buffer_Address : constant System.Address := Decision_Buffer'Address;
   pragma Export (C, Decision_Buffer_Address, "xcov__buf_dc__S_pkg");

   MCDC_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
   pragma Export (C, MCDC_Buffer_Address, "xcov__buf_mcdc__S_pkg");

   Unit_Name : constant String := "pkg";
   Project_Name : constant String := "";

   Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
     (Fingerprint => (172, 135, 120, 131, 81, 65, 170, 218, 156, 87, 154, 30, 35, 71, 96, 166, 14, 161, 125, 216),
      Language  => Unit_Based_Language,
      Unit_Part => Unit_Spec,
      Unit_Name => (Unit_Name'Address, Unit_Name'Length),
      Project_Name => (Project_Name'Address, Project_Name'Length),
      Bit_Maps_Fingerprint => (18, 188, 239, 54, 111, 156, 46, 122, 240, 90, 103, 226, 23, 122, 35, 87, 156, 185, 141, 194),
      Statement => Statement_Buffer'Address,
      Decision  => Decision_Buffer'Address,
      MCDC      => MCDC_Buffer'Address,
      Statement_Last_Bit => -1,
      Decision_Last_Bit => -1,
      MCDC_Last_Bit => -1);

   Buffers_Group : aliased constant Coverage_Buffers_Group := (1 => Buffers'Access);
   C_Buffers_Group : aliased constant GNATcov_RTS_Coverage_Buffers_Group := (1, Buffers_Group'Address);
      pragma Export (C, C_Buffers_Group, "gnatcov_rts_buffers_S_pkg_buffers");

end GNATcov_RTS.Buffers.BS_pkg;
