pragma Style_Checks (Off); pragma Warnings (Off);
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
with GCVRT.Bmain_1;
with GCVRT.Bmain_2;
with GCVRT.Bpkg;

package GCVRT.Foo is

   pragma Preelaborate;

   gnatcov_rts_buffers_main_1_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_main_1_buffers,"gnatcov_rts_buffers_main_1_buffers");
   gnatcov_rts_buffers_main_2_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_main_2_buffers,"gnatcov_rts_buffers_main_2_buffers");
   gnatcov_rts_buffers_pkg_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_pkg_buffers,"gnatcov_rts_buffers_pkg_buffers");
   List : constant GNATcov_RTS.Buffers.Lists.Coverage_Buffers_Group_Array := (
      1 => gnatcov_rts_buffers_main_1_buffers'Access,
      2 => gnatcov_rts_buffers_main_2_buffers'Access,
      3 => gnatcov_rts_buffers_pkg_buffers'Access);
   C_List : constant GNATcov_RTS.Buffers.Lists.GNATcov_RTS_Coverage_Buffers_Group_Array :=
      ( 3, List'Address);
   pragma Export (C, C_List, "gnatcov_rts_buffers_array_Foo");

end GCVRT.Foo;
