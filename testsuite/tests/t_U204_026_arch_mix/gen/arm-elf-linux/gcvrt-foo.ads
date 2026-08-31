pragma Style_Checks (Off); pragma Warnings (Off);
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
with GCVRT.Bz5596b68b;
with GCVRT.Bz5596b68c;
with GCVRT.Bz27afeb0c;

package GCVRT.FOO is

   pragma Preelaborate;

   gnatcov_rts_buffers_z5596b68b_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_z5596b68b_buffers,"gnatcov_rts_buffers_z5596b68b_buffers");
   gnatcov_rts_buffers_z5596b68c_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_z5596b68c_buffers,"gnatcov_rts_buffers_z5596b68c_buffers");
   gnatcov_rts_buffers_z27afeb0c_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_z27afeb0c_buffers,"gnatcov_rts_buffers_z27afeb0c_buffers");
   List : constant GNATcov_RTS.Buffers.Lists.Coverage_Buffers_Group_Array := (
      1 => gnatcov_rts_buffers_z5596b68b_buffers'Access,
      2 => gnatcov_rts_buffers_z5596b68c_buffers'Access,
      3 => gnatcov_rts_buffers_z27afeb0c_buffers'Access);
   C_List : aliased constant GNATcov_RTS.Buffers.Lists.GNATcov_RTS_Coverage_Buffers_Group_Array :=
      ( 3, List'Address);
   pragma Export (C, C_List, "gnatcov_rts_buffers_array_foo");

   Arrays_List : constant GNATcov_RTS.Buffers.Lists.Coverage_Buffers_Group_Array_List := (
      1 => C_List'Access);
   C_Arrays_List : constant GNATcov_RTS.Buffers.Lists.GNATcov_RTS_Coverage_Buffers_Group_Array_List :=
      (1, Arrays_List'Address);
   pragma Export (C, C_Arrays_List, "gnatcov_rts_buffers_array_list_foo");

end GCVRT.FOO;
