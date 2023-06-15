pragma Style_Checks (Off); pragma Warnings (Off);
with GNATcov_RTS.Buffers.BB_main_1;
with GNATcov_RTS.Buffers.BB_main_2;
with GNATcov_RTS.Buffers.BB_pkg;
with GNATcov_RTS.Buffers.BS_pkg;

package GNATcov_RTS.Buffers.Lists.foo is

   pragma Preelaborate;

   gnatcov_rts_buffers_B_main_1_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_B_main_1_buffers,"gnatcov_rts_buffers_B_main_1_buffers");
   gnatcov_rts_buffers_B_main_2_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_B_main_2_buffers,"gnatcov_rts_buffers_B_main_2_buffers");
   gnatcov_rts_buffers_B_pkg_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_B_pkg_buffers,"gnatcov_rts_buffers_B_pkg_buffers");
   gnatcov_rts_buffers_S_pkg_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_S_pkg_buffers,"gnatcov_rts_buffers_S_pkg_buffers");
   List : constant GNATcov_RTS.Buffers.Lists.Coverage_Buffers_Group_Array := (
      1 => gnatcov_rts_buffers_B_main_1_buffers'Access,
      2 => gnatcov_rts_buffers_B_main_2_buffers'Access,
      3 => gnatcov_rts_buffers_B_pkg_buffers'Access,
      4 => gnatcov_rts_buffers_S_pkg_buffers'Access);

end GNATcov_RTS.Buffers.Lists.foo;
