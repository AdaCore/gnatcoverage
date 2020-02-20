--  This unit needs to be compilable with Ada 2005 compilers

with Ada.Text_IO;

package GNATcov_RTS.Traces.Output.Base64 is

   procedure Write_Trace_File
     (Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String;
      Exec_Date    : Serialized_Timestamp;
      User_Data    : String := "");
   --  Write a Base64-encoded trace file to the standard output
   --  (Ada.Text_IO.Standard_Output).

end GNATcov_RTS.Traces.Output.Base64;
