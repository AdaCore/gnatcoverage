pragma Style_Checks (Off); pragma Warnings (Off);
with Interfaces.C; use Interfaces.C;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
package GNATcov_RTS.Buffers.BB_main_1 is

   pragma Preelaborate;

   Statement_Buffer : Coverage_Buffer_Type (0 .. 0) := (others => False);
   Statement_Buffer_Address : constant System.Address := Statement_Buffer'Address;
   pragma Export (C, Statement_Buffer_Address, "xcov__buf_stmt__B_main_1");

   Decision_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   Decision_Buffer_Address : constant System.Address := Decision_Buffer'Address;
   pragma Export (C, Decision_Buffer_Address, "xcov__buf_dc__B_main_1");

   MCDC_Buffer : Coverage_Buffer_Type (0 .. -1) := (others => False);
   MCDC_Buffer_Address : constant System.Address := MCDC_Buffer'Address;
   pragma Export (C, MCDC_Buffer_Address, "xcov__buf_mcdc__B_main_1");

   Unit_Name : constant String := "main_1";
   Project_Name : constant String := "";

   Buffers : aliased constant GNATcov_RTS_Coverage_Buffers :=
     (Fingerprint => (125, 27, 0, 113, 188, 140, 235, 155, 190, 125, 180, 238, 139, 95, 108, 198, 229, 72, 222, 11),
      Language  => Unit_Based_Language,
      Unit_Part => Unit_Body,
      Unit_Name => (Unit_Name'Address, Unit_Name'Length),
      Project_Name => (Project_Name'Address, Project_Name'Length),
      Bit_Maps_Fingerprint => (242, 233, 177, 172, 181, 126, 10, 107, 129, 130, 58, 85, 69, 52, 28, 242, 228, 191, 81, 252),
      Statement => Statement_Buffer'Address,
      Decision  => Decision_Buffer'Address,
      MCDC      => MCDC_Buffer'Address,
      Statement_Last_Bit => 0,
      Decision_Last_Bit => -1,
      MCDC_Last_Bit => -1);

   Buffers_Group : aliased constant Coverage_Buffers_Group := (1 => Buffers'Access);
   C_Buffers_Group : aliased constant GNATcov_RTS_Coverage_Buffers_Group := (1, Buffers_Group'Address);
      pragma Export (C, C_Buffers_Group, "gnatcov_rts_buffers_B_main_1_buffers");

end GNATcov_RTS.Buffers.BB_main_1;
