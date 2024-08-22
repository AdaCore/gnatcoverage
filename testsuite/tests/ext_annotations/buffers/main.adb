with Pkg;
with Pkh;

procedure Main is
   procedure Do_CPP_Dump;
   pragma Import (C, Do_CPP_Dump, "do_cpp_dump");

   procedure Do_C_Dump;
   pragma Import (C, Do_C_Dump, "do_c_dump");
begin
   Do_C_Dump;
   Do_CPP_Dump;
   Pkg.Do_Dump;
   Pkh.Do_Dump;
end Main;
