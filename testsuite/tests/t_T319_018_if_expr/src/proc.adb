pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Pkg; use Pkg;

procedure Proc (In_P : In_Data_Port'Class)
is
   Orig_Out_P : constant Out_Data_Port'Class :=                -- # uncond
     (if In_P.Incoming_Signal.Is_Not_Null                      -- # decision
      then Out_Data_Port'Class (In_P.Incoming_Signal.Src_Port) -- # out-true
      else Null_Out_Data_Port);                                -- # out-false
begin
   Put_Line (Orig_Out_P.Image);                                -- # uncond
end Proc;
