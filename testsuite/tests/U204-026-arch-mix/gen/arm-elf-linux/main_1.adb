pragma Style_Checks (Off); pragma Warnings (Off);with Pkg;

with GNATcov_RTS.Buffers;with GCVRT.Pz794ac68b;with GCVRT.DB_z794ac68b;with GNATcov_RTS;with GNATcov_RTS.Buffers;pragma Compile_Time_Error(GNATcov_RTS.Version/=8,"Incompatible GNATcov_RTS version, please use the GNATcov_RTS project provided with your GNATcoverage distribution.");procedure Main_1 is
begin
   GNATcov_RTS.Buffers.Witness(GCVRT.Pz794ac68b.Buffers_1.Statement_Buffer,0);Pkg.Compute (True, False);
GCVRT.DB_z794ac68b.Dump_Buffers;end Main_1;
