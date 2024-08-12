pragma Style_Checks (Off); pragma Warnings (Off);
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
with GCVRT.Bz794ac68b;
with GCVRT.Bz794ac68c;
with GCVRT.Bz3791e32c;

package GCVRT.Foo is

   pragma Preelaborate;

   gnatcov_rts_buffers_z794ac68b_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_z794ac68b_buffers,"gnatcov_rts_buffers_z794ac68b_buffers");
   gnatcov_rts_buffers_z794ac68c_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_z794ac68c_buffers,"gnatcov_rts_buffers_z794ac68c_buffers");
   gnatcov_rts_buffers_z3791e32c_buffers : aliased constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Import (C, gnatcov_rts_buffers_z3791e32c_buffers,"gnatcov_rts_buffers_z3791e32c_buffers");
   List : constant GNATcov_RTS.Buffers.Lists.Coverage_Buffers_Group_Array := (
      1 => gnatcov_rts_buffers_z794ac68b_buffers'Access,
      2 => gnatcov_rts_buffers_z794ac68c_buffers'Access,
      3 => gnatcov_rts_buffers_z3791e32c_buffers'Access);
   C_List : constant GNATcov_RTS.Buffers.Lists.GNATcov_RTS_Coverage_Buffers_Group_Array :=
      ( 3, List'Address);
   pragma Export (C, C_List, "gnatcov_rts_buffers_array_foo");

end GCVRT.Foo;
