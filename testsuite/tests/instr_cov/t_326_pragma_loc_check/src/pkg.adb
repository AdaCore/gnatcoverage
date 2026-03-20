pragma Annotate (Xcov, Dump_Buffers);
package body Pkg is

   procedure Wrong_Do_Dump is
      pragma Annotate (Xcov, Dump_Buffers);
   begin
      null;  -- # st
   end Wrong_Do_Dump;

end Pkg;
