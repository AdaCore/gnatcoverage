pragma Style_Checks (Off); pragma Warnings (Off);with Pkg;

with GNATcov_RTS.Buffers;with GCVRT.Pz794ac68c;with GCVRT.DB_z794ac68c;with GNATcov_RTS;with GNATcov_RTS.Buffers;pragma Compile_Time_Error(GNATcov_RTS.Version/=5,"Incompatible GNATcov_RTS version, please use the GNATcov_RTS project provided with your GNATcoverage distribution.");procedure Main_2 is
GNATcov_Dump_Object:GCVRT.DB_z794ac68c.Dump_Controlled_Type;begin
   GNATcov_RTS.Buffers.Witness(GCVRT.Pz794ac68c.Buffers_1.Statement_Buffer,0);Pkg.Compute (True, True);
end Main_2;
